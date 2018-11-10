/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package network.tcp

import akka.actor.Actor.Receive
import akka.actor.{Actor, ActorContext, ActorRef}
import akka.io.Tcp.{PeerClosed, Received, Write}
import akka.util.ByteString
import network.message.{Message, RawMessage}

case class TcpConnectionHandler(tcpConnection: TcpConnection, context : ActorContext, tcpManager : ActorRef, tcpConnectionManager : ActorRef, btcProtocolHandler: ActorRef) {
  protected var inputStream = ByteString()

  def receive: Receive = {
    case Received(data) =>
      inputStream = inputStream ++ data

      var moreMessages = true
      while (moreMessages) {
        RawMessage.fromBytesOpt(inputStream) match {
          case None =>
            moreMessages = false
          case Some((rawMessage, remainingInputStream)) =>
            Message.fromRawMessage(rawMessage) match {
              case Left(malformed) =>
                // println(s"Received malformed: $remote $malformed")
                btcProtocolHandler ! malformed
              case Right(message) =>
                // println(s"Received: $remote $message")
                btcProtocolHandler ! message
            }
            inputStream = remainingInputStream
        }
      }

    case message : Message =>
      //println(s"Sending:  $remote $message")
      tcpManager ! Write(message.toBytes)

    case TcpConnection.SendBytes(bs) =>
      //println(s"Sending:  $remote $bs")
      tcpManager ! Write(bs)

    case PeerClosed =>
      // will stop handler child too
      tcpConnectionManager ! TcpConnectionManager.Unregister(tcpConnection)
      context stop tcpConnection.self

    case other =>
      // println(s"Received: $remote $other")
      ;
  }
}
