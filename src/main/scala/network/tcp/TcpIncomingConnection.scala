/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package network.tcp

import java.net.InetSocketAddress

import akka.actor.{ActorRef, Props}
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import network.btc.BtcNode

object TcpIncomingConnection {
  def props(id : Int, btcNode: BtcNode, local: InetSocketAddress, remote: InetSocketAddress, createSubscriber : TcpConnection => ActorRef, tcpManager : ActorRef) =
    Props(classOf[TcpIncomingConnection], id, btcNode, local, remote, createSubscriber, tcpManager)
}

case class TcpIncomingConnection(id : Int, btcNode: BtcNode, local: InetSocketAddress, remote: InetSocketAddress, createSubscriber: TcpConnection => ActorRef, tcpManager: ActorRef) extends TcpConnectionHandler {
  protected val log = btcNode.log
  IO(Tcp)(btcNode.actorSystem) ! Connect(remote)

  log.info(s"Successful incoming connection from $remote")
  tcpManager ! Register(self)

  private val tcpConnection = TcpConnection(id, self, local, remote)
  btcNode.tcpConnectionManager ! TcpConnectionManager.Register(tcpConnection)

  private val subscriber = createSubscriber(tcpConnection)

  def receive: Receive =
    super.receive(tcpConnection, tcpManager, subscriber)
}