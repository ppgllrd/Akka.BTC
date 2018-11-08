/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package network.tcp

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, Props}
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import akka.util.ByteString
import network.btc.BtcNode
import network.message.{Message, RawMessage}

object TcpIncomingConnection {
  def props(btcNode: BtcNode, local: InetSocketAddress, remote: InetSocketAddress, createHandler : TcpConnection => ActorRef, tcpManager : ActorRef) =
    Props(classOf[TcpIncomingConnection], btcNode, local, remote, createHandler, tcpManager)
}

case class TcpIncomingConnection(btcNode: BtcNode, local: InetSocketAddress, remote: InetSocketAddress, createHandler: TcpConnection => ActorRef, tcpManager: ActorRef) extends Actor {
  private val actorSystem = btcNode.actorSystem

  IO(Tcp)(actorSystem) ! Connect(remote)

  private var inputStream = ByteString()

  println(s"Successful incoming connection from $remote")
  tcpManager ! Register(self)

  val tcpConnection = TcpConnection(self, local, remote)
  btcNode.tcpConnectionManager ! TcpConnectionManager.Register(tcpConnection)

  val handler =
    createHandler(tcpConnection)

  def receive: Receive = {
    case Received(data) =>
      inputStream = inputStream ++ data

      var moreMessages = true
      while(moreMessages) {
        RawMessage.fromBytesOpt(inputStream) match {
          case None =>
            moreMessages = false
          case Some((rawMessage, remainingInputStream)) =>
            val message = Message.fromRawMessage(rawMessage)
            println("Received: "+remote+" "+message)
            inputStream = remainingInputStream
            handler ! message
        }
      }

    case message : Message =>
      println("Sending:  "+remote+" "+message)
      tcpManager ! Write(message.toBytes)

    case TcpConnection.SendBytes(bs) =>
      println("Sending:  "+remote+" "+bs)
      tcpManager ! Write(bs)

    case PeerClosed =>
      // will stop handler child too
      context stop self

    case other =>
      println("Received: "+remote+" "+other)
  }
}