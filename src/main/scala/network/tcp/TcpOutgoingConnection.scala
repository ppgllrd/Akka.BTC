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

object TcpOutgoingConnection {
  def props(id: Int, btcNode: BtcNode, remote: InetSocketAddress, createHandler : TcpConnection => ActorRef) =
    Props(classOf[TcpOutgoingConnection], id, btcNode, remote, createHandler)
}

case class TcpOutgoingConnection(id: Int, btcNode: BtcNode, remote: InetSocketAddress, createHandler: TcpConnection => ActorRef) extends Actor {
  private val log = btcNode.log
  IO(Tcp)(btcNode.actorSystem) ! Connect(remote)

  def receive = {
    case CommandFailed(command: Tcp.Command) =>
      log.info(s"Failed to connect to $remote $command")
      context stop self

    case Connected(remote, local) =>
      log.info(s"Successful outgoing connection to $remote from $local")

      val tcpManager = sender()
      tcpManager ! Register(self)

      val tcpConnection = TcpConnection(id, self, local, remote)
      btcNode.tcpConnectionManager ! TcpConnectionManager.Register(tcpConnection)

      val handler = createHandler(tcpConnection)

      val tcpConnectionHandler = TcpConnectionHandler(tcpConnection, btcNode, context, tcpManager, handler)

      context become tcpConnectionHandler.receive
  }
}



