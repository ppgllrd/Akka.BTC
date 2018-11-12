/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package network.tcp

import akka.actor.Actor.Receive
import akka.actor.{ActorContext, ActorRef}
import akka.io.Tcp.{PeerClosed, Received, Write}
import akka.util.ByteString
import network.btc.BtcNode
import network.message.{Message, RawMessage}

case class TcpConnectionHandler(tcpConnection: TcpConnection, btcNode: BtcNode, context : ActorContext, tcpManager : ActorRef, btcProtocolHandler: ActorRef) {
  protected var inputStream = ByteString()
  private val log = btcNode.log

  def receive: Receive = {
    case Received(data) =>
      inputStream = inputStream ++ data

      var moreMessages = true
      while (moreMessages) {
        RawMessage.parserOpt(inputStream) match {
          case (_, None) =>
            moreMessages = false
          case (remainingInputStream, Some(rawMessage)) =>
            Message.fromRawMessage(rawMessage) match {
              case Left(malformed) =>
                log.info(s"Received malformed: ${tcpConnection.remote} $malformed")
                btcProtocolHandler ! malformed
              case Right(message) =>
                log.info(s"Received: ${tcpConnection.remote} $message")
                btcProtocolHandler ! message
            }
            inputStream = remainingInputStream
        }
      }

    case message : Message =>
      log.info(s"Sending: ${tcpConnection.remote} $message")
      tcpManager ! Write(message.toBytes)

    case TcpConnection.SendBytes(bs) =>
      log.info(s"Sending: ${tcpConnection.remote} $bs")
      tcpManager ! Write(bs)

    case PeerClosed =>
      log.info(s"PeerClosed: ${tcpConnection.remote}")
      // will stop handler child too
      btcNode.tcpConnectionManager ! TcpConnectionManager.Unregister(tcpConnection)
      context stop tcpConnection.self

    case other =>
      log.info(s"Received: ${tcpConnection.remote} $other")
  }
}
