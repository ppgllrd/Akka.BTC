/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package network.tcp

import akka.actor.{Actor, ActorRef}
import akka.event.LoggingAdapter
import akka.io.Tcp.{PeerClosed, Received, Write}
import akka.util.ByteString
import network.btc.BtcNode
import network.message.{Message, RawMessage}

trait TcpConnectionHandler extends Actor {
  val btcNode: BtcNode
  protected val log : LoggingAdapter

  private var inputStream = ByteString()

  def receive(tcpConnection: TcpConnection, tcpManager: ActorRef, subscriber: ActorRef): Receive = {
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
                subscriber ! malformed
              case Right(message) =>
                log.info(s"Received: ${tcpConnection.remote} $message")
                subscriber ! message
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
      btcNode.tcpConnectionManager ! TcpConnectionManager.Closed(tcpConnection)
      context stop tcpConnection.self

    case other =>
      log.info(s"Received: ${tcpConnection.remote} $other")
  }
}