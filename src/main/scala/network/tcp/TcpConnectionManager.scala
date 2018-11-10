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
import network.tcp.TcpConnectionManager.{CreateIncomingConnection, CreateOutgoingConnection, Register, Unregister}

import scala.collection.mutable.ArrayBuffer

object TcpConnectionManager {
  def props(btcNode: BtcNode) =
    Props(classOf[TcpConnectionManager], btcNode)

  case class Register(tcpConnection : TcpConnection)

  case class Unregister(tcpConnection : TcpConnection)

  case class CreateOutgoingConnection(remote: InetSocketAddress)

  case class CreateIncomingConnection(local: InetSocketAddress, remote: InetSocketAddress, tcpManager: ActorRef)
}

case class TcpConnectionManager(btcNode: BtcNode) extends Actor {
  private val actorSystem = btcNode.actorSystem

  private val openedTcpConnections = scala.collection.mutable.Set[TcpConnection]()
  private var nextTcpId = 0

  private def isOpened(remote : InetSocketAddress) : Boolean =
    openedTcpConnections.map(_.remote).contains(remote)


  def receive: Receive = {
    case Register(tcpConnection) =>
      openedTcpConnections += tcpConnection

    case Unregister(tcpConnection) =>
      openedTcpConnections -= tcpConnection

    case CreateOutgoingConnection(remote) =>
      if(!isOpened(remote)) {
        def newHandler(tcpConnection: TcpConnection) =
          actorSystem.actorOf(BtcOutgoingConnection.props(btcNode, tcpConnection))

        actorSystem.actorOf(TcpOutgoingConnection.props(nextTcpId, btcNode, remote, newHandler))
        nextTcpId += 1
      }
      println(openedTcpConnections.size)

    case CreateIncomingConnection(local, remote, tcpManager) =>
      def newHandler(tcpConnection : TcpConnection) =
        actorSystem.actorOf(BtcIncomingConnection.props(btcNode, tcpConnection))

      actorSystem.actorOf(TcpIncomingConnection.props(nextTcpId, btcNode, local, remote, newHandler, tcpManager))
      nextTcpId += 1
  }
}
