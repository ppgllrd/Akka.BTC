/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package network.tcp

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import network.btc.{BtcIncomingConnection, BtcOutgoingConnection}
import network.tcp.TcpConnectionManager.{CreateIncomingConnection, CreateOutgoingConnection}

import scala.collection.mutable.ArrayBuffer

object TcpConnectionManager {
  def props(actorSystem: ActorSystem) =
    Props(classOf[TcpConnectionManager], actorSystem)

  case class CreateOutgoingConnection(inetSocketAddress: InetSocketAddress)

  case class CreateIncomingConnection(inetSocketAddress: InetSocketAddress)
}

case class TcpConnectionManager(actorSystem: ActorSystem) extends Actor {
  private val allTcpConnections = ArrayBuffer[ActorRef]()

  def receive: Receive = {
    case CreateOutgoingConnection(inetSocketAddress) =>
      def newHandler(tcpConnection : TcpConnection) =
        actorSystem.actorOf(BtcOutgoingConnection.props(tcpConnection, actorSystem))
      val tcpConnection = actorSystem.actorOf(TcpConnection.props(inetSocketAddress, actorSystem, newHandler))
      allTcpConnections.append(tcpConnection)

    case CreateIncomingConnection(inetSocketAddress) =>
      def newHandler(tcpConnection : TcpConnection) =
        actorSystem.actorOf(BtcIncomingConnection.props(tcpConnection, actorSystem))
      val tcpConnection = actorSystem.actorOf(TcpConnection.props(inetSocketAddress, actorSystem, newHandler))
      allTcpConnections.append(tcpConnection)
  }
}
