/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package network.tcp

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ActorSystem, Kill, Props}
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import akka.util.ByteString
import network.message.{Message, RawMessage}
import network.btc.BtcOutgoingConnection

object TcpConnection {
  def props(inetSocketAddress: InetSocketAddress, actorSystem: ActorSystem, createHander : TcpConnection => ActorRef) =
    Props(classOf[TcpConnection], inetSocketAddress, actorSystem, createHander)

  case class SendBytes(bs : ByteString)
}

case class TcpConnection(inetSocketAddress: InetSocketAddress, actorSystem: ActorSystem, createHander : TcpConnection => ActorRef) extends Actor {
  IO(Tcp)(actorSystem) ! Connect(inetSocketAddress)

  private var inputStream = ByteString()

  var remote : Option[InetSocketAddress] = None
  var local : Option[InetSocketAddress] = None

  def receive = {
    case CommandFailed(command: Tcp.Command) =>
      println("Failed to connect to " + inetSocketAddress)
      self ! Kill

    case Connected(remote, local) =>
      this.remote = Some(remote)
      this.local = Some(local)

      println("Successfully connected to " + inetSocketAddress)
      val tcpManager = sender()
      tcpManager ! Register(self)

      val btcConnection = actorSystem.actorOf(BtcOutgoingConnection.props(this, actorSystem))

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
                println("Received: "+inetSocketAddress+" "+message)
                inputStream = remainingInputStream
                btcConnection ! message
            }
          }

        case message : Message =>
          println("Sending:  "+inetSocketAddress+" "+message)
          tcpManager ! Write(message.toBytes)

        case TcpConnection.SendBytes(bs) =>
          println("Sending:  "+inetSocketAddress+" "+bs)
          tcpManager ! Write(bs)

        case other =>
          println("Received: "+inetSocketAddress+" "+other)
      }
  }
}



