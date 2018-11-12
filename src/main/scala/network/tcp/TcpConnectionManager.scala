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
import network.tcp.TcpConnectionManager._

object TcpConnectionManager {
  def props(btcNode: BtcNode) =
    Props(classOf[TcpConnectionManager], btcNode)

  case class Opened(tcpConnection : TcpConnection)

  case class CouldNotOpen(remote : InetSocketAddress)

  case class Closed(tcpConnection : TcpConnection)

  case class CreateOutgoingConnection(remote: InetSocketAddress)

  case class CreateIncomingConnection(local: InetSocketAddress, remote: InetSocketAddress, tcpManager: ActorRef)
}

case class TcpConnectionManager(btcNode: BtcNode) extends Actor {
  private val actorSystem = btcNode.actorSystem

  private val openedTcpConnections = scala.collection.mutable.Set[TcpConnection]()
  private var nextTcpId = 0

  val tryingToConnectTo = scala.collection.mutable.Set[InetSocketAddress]()

  private def isOpened(remote : InetSocketAddress) : Boolean =
    !tryingToConnectTo.contains(remote) &&
    openedTcpConnections.map(_.remote).contains(remote)

  def receive: Receive = {
    case CouldNotOpen(remote) =>
      tryingToConnectTo -= remote

    case Opened(tcpConnection) =>
      tryingToConnectTo -= tcpConnection.remote
      openedTcpConnections += tcpConnection

    case Closed(tcpConnection) =>
      openedTcpConnections -= tcpConnection

    case CreateOutgoingConnection(remote) =>
      if(!isOpened(remote)) {
        def newSubscriber(tcpConnection: TcpConnection) =
          actorSystem.actorOf(BtcOutgoingConnection.props(btcNode, tcpConnection))

        tryingToConnectTo.add(remote)
        actorSystem.actorOf(TcpOutgoingConnection.props(nextTcpId, btcNode, remote, newSubscriber))
        nextTcpId += 1
      }

    case CreateIncomingConnection(local, remote, tcpManager) =>
      if(!isOpened(remote)) {
        def newSubscriber(tcpConnection: TcpConnection) =
          actorSystem.actorOf(BtcIncomingConnection.props(btcNode, tcpConnection))

        tryingToConnectTo.add(remote)
        actorSystem.actorOf(TcpIncomingConnection.props(nextTcpId, btcNode, local, remote, newSubscriber, tcpManager))
        nextTcpId += 1
      }
  }
}
