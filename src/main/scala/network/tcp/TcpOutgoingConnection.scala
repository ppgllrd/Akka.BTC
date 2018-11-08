/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package network.tcp

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, Kill, Props}
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import akka.util.ByteString
import network.btc.BtcNode
import network.message.{Message, RawMessage}

object TcpOutgoingConnection {
  def props(btcNode: BtcNode, remote: InetSocketAddress, createHandler : TcpConnection => ActorRef) =
    Props(classOf[TcpOutgoingConnection], btcNode, remote, createHandler)
}

case class TcpOutgoingConnection(btcNode: BtcNode, remote: InetSocketAddress, createHandler: TcpConnection => ActorRef) extends Actor {
  private val actorSystem = btcNode.actorSystem
  IO(Tcp)(btcNode.actorSystem) ! Connect(remote)

  private var inputStream = ByteString()

  def receive = {
    case CommandFailed(command: Tcp.Command) =>
      println("Failed to connect to " + remote)
      self ! Kill

    case Connected(remote, local) =>

      println("Successfully connected to " + remote)
      println(local, remote)
      println()
      val tcpManager = sender()
      tcpManager ! Register(self)

      val tcpConnection = TcpConnection(self, local, remote)
      btcNode.tcpConnectionManager ! TcpConnectionManager.Register(tcpConnection)

      val handler =
       createHandler(tcpConnection)

      context become {
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
}



