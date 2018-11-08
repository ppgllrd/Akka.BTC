/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package network.tcp

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, Props}
import network.btc.{BtcIncomingConnection, BtcNode, BtcOutgoingConnection}
import network.tcp.TcpConnectionManager.{CreateIncomingConnection, CreateOutgoingConnection, Register}

import scala.collection.mutable.ArrayBuffer

object TcpConnectionManager {
  def props(btcNode: BtcNode) =
    Props(classOf[TcpConnectionManager], btcNode)

  case class Register(tcpConnection : TcpConnection)

  case class CreateOutgoingConnection(remote: InetSocketAddress)

  case class CreateIncomingConnection(local: InetSocketAddress, remote: InetSocketAddress, tcpManager: ActorRef)
}

case class TcpConnectionManager(btcNode: BtcNode) extends Actor {
  private val actorSystem = btcNode.actorSystem

  private val allTcpConnections = ArrayBuffer[TcpConnection]()

  def receive: Receive = {
    case Register(tcpConnection) =>
      allTcpConnections.append(tcpConnection)

    case CreateOutgoingConnection(remote) =>
      def newHandler(tcpConnection : TcpConnection) =
        actorSystem.actorOf(BtcOutgoingConnection.props(btcNode, tcpConnection))

      actorSystem.actorOf(TcpOutgoingConnection.props(btcNode, remote, newHandler))

    case CreateIncomingConnection(local, remote, tcpManager) =>
      def newHandler(tcpConnection : TcpConnection) =
        actorSystem.actorOf(BtcIncomingConnection.props(btcNode, tcpConnection))

      actorSystem.actorOf(TcpIncomingConnection.props(btcNode, local, remote, newHandler, tcpManager))
  }
}
