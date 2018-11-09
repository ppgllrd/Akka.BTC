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
import network.btc.BtcNode

object TcpIncomingConnection {
  def props(btcNode: BtcNode, local: InetSocketAddress, remote: InetSocketAddress, createHandler : TcpConnection => ActorRef, tcpManager : ActorRef) =
    Props(classOf[TcpIncomingConnection], btcNode, local, remote, createHandler, tcpManager)
}

case class TcpIncomingConnection(btcNode: BtcNode, local: InetSocketAddress, remote: InetSocketAddress, createHandler: TcpConnection => ActorRef, tcpManager: ActorRef) extends Actor {
  IO(Tcp)(btcNode.actorSystem) ! Connect(remote)

  println(s"Successful incoming connection from $remote")
  tcpManager ! Register(self)

  val tcpConnection = TcpConnection(self, local, remote)
  btcNode.tcpConnectionManager ! TcpConnectionManager.Register(tcpConnection)

  val handler = createHandler(tcpConnection)

  val tcpConnectionHandler = TcpConnectionHandler(this, tcpManager, handler)

  def receive: Receive =
    tcpConnectionHandler.receive
}