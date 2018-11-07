/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package network.tcp

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.io.Tcp.{Bind, Bound, CommandFailed}
import akka.io.{IO, Tcp}
import network.btc.BtcNode

object TcpServer {
  def props(actorSystem: ActorSystem, tcpConnectionManager: ActorRef) =
    Props(classOf[TcpServer], actorSystem,tcpConnectionManager)
}

case class TcpServer(actorSystem: ActorSystem, tcpConnectionManager: ActorRef) extends Actor {
  import context.system
  IO(Tcp) ! Tcp.Bind(self, new InetSocketAddress("127.0.0.1", BtcNode.tcpServerPort))
  def receive = {
    case b @ Bound(localAddress) =>
      println(s"TcpServer listening on $localAddress")

    case CommandFailed(_: Bind) =>
      context stop self

    case Tcp.Connected(remote, local) =>
      println(s"New incoming connection from $remote")
      tcpConnectionManager ! TcpConnectionManager.CreateIncomingConnection(remote)
  }
}
