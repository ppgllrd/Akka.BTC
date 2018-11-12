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
import network.tcp.TcpConnectionManager.CouldNotOpen

object TcpOutgoingConnection {
  def props(id: Int, btcNode: BtcNode, remote: InetSocketAddress, createSubscriber : TcpConnection => ActorRef) =
    Props(classOf[TcpOutgoingConnection], id, btcNode, remote, createSubscriber)
}

case class TcpOutgoingConnection(id: Int, btcNode: BtcNode, remote: InetSocketAddress, createSubscriber: TcpConnection => ActorRef) extends TcpConnectionHandler {
  protected val log = btcNode.log
  IO(Tcp)(btcNode.actorSystem) ! Connect(remote)

  def receive = {
    case CommandFailed(command: Tcp.Command) =>
      log.info(s"Failed to connect to $remote $command")
      btcNode.tcpConnectionManager ! CouldNotOpen(remote)
      context stop self

    case Connected(remote, local) =>
      log.info(s"Successful outgoing connection to $remote from $local")

      val tcpManager = sender()
      tcpManager ! Register(self)

      val tcpConnection = TcpConnection(id, self, local, remote)
      btcNode.tcpConnectionManager ! TcpConnectionManager.Opened(tcpConnection)

      val subscriber = createSubscriber(tcpConnection)

      context become super.receive(tcpConnection, tcpManager, subscriber)
  }
}



