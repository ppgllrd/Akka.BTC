/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package network.tcp

import akka.actor.{Actor, Props}
import akka.io.Tcp.{Bind, Bound, CommandFailed}
import akka.io.{IO, Tcp}
import network.btc.BtcNode

object TcpServer {
  def props(btcNode : BtcNode) =
    Props(classOf[TcpServer], btcNode)
}

case class TcpServer(btcNode : BtcNode) extends Actor {
  import context.system
  private val tcpConnectionManager = btcNode.tcpConnectionManager

  IO(Tcp) ! Tcp.Bind(self, BtcNode.tcpServerAddress)

  def receive = {
    case Bound(localAddress) =>
      println(s"TcpServer listening on $localAddress")

    case CommandFailed(what: Bind) =>
      println(s"TcpServer failed $what")
      context stop self

    case Tcp.Connected(remote, local) =>
      println(s"New incoming connection from $remote")
      val tcpManager = sender()
      tcpConnectionManager ! TcpConnectionManager.CreateIncomingConnection(local, remote, tcpManager)
  }
}
