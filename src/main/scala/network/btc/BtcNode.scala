/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package network.btc

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorSystem, Props}
import akka.event.{Logging, LoggingAdapter}
import network.message.{Magic, Services}
import network.tcp.{TcpConnectionManager, TcpServer}

object BtcNode {
  val magic : Int = Magic.mainnet
  val services : Long = Services.NODE_NETWORK
  val version : Int = 70015
  val userAgent = "ScalaBTC:0.0.1"

  val tcpServerAddress = new InetSocketAddress("127.0.0.1", 8333)
}

case class BtcNode(actorSystem: ActorSystem, log : LoggingAdapter) {
  val tcpConnectionManager = actorSystem.actorOf(TcpConnectionManager.props(this))
  val tcpServer = actorSystem.actorOf(TcpServer.props(this))
  val networkAddresses = actorSystem.actorOf(NetworkAddresses.props(this))
}

object BtcNodeActor {
  def props(actorSystem: ActorSystem) =
    Props(classOf[BtcNodeActor], actorSystem)
}

case class BtcNodeActor(actorSystem: ActorSystem) extends Actor {
  val log = Logging(context.system, this)
  val btcNode = BtcNode(actorSystem, log)

  val remote1 = new InetSocketAddress("54.36.61.219", 8333)
  val remote2 = new InetSocketAddress("82.74.137.168", 8333)
  val remote3 = new InetSocketAddress("183.193.213.254", 8333)
  val remote4 = new InetSocketAddress("34.237.15.134", 8333)
  val remote5 = new InetSocketAddress("183.193.213.254", 8333)
  val remotes = List(remote1, remote2, remote3, remote4, remote5)

  for(remote <- remotes)
    btcNode.tcpConnectionManager ! TcpConnectionManager.CreateOutgoingConnection(remote)

  val remote6 = BtcNode.tcpServerAddress
  // btcNode.tcpConnectionManager ! TcpConnectionManager.CreateOutgoingConnection(remote6)

  override def receive: Receive = {
    case _ =>
      ;
  }
}