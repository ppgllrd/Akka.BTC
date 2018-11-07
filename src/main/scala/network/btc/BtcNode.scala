/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package network.btc

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorSystem, Props}
import network.message.{Magic, Services}
import network.tcp.{TcpConnectionManager, TcpServer}

object BtcNode {
  val magic : Int = Magic.mainnet
  val services : Long = Services.NODE_NETWORK
  val version : Int = 70005
  val userAgent = "ScalaBTC:0.0.1"

  val tcpServerPort : Int = 8333

  def props(actorSystem: ActorSystem) =
    Props(classOf[BtcNode], actorSystem)
}


case class BtcNode(actorSystem: ActorSystem) extends Actor {
  val tcpConnectionManager = actorSystem.actorOf(TcpConnectionManager.props(actorSystem))
  val tcpServer = actorSystem.actorOf(TcpServer.props(actorSystem, tcpConnectionManager))

  val remote1 = new InetSocketAddress("54.36.61.219", 8333)
  val remote2 = new InetSocketAddress("82.74.137.168", 8333)
  val remote3 = new InetSocketAddress("183.193.213.254", 8333)
  val remote4 = new InetSocketAddress("34.237.15.134", 8333)
  val remote5 = new InetSocketAddress("183.193.213.254", 8333)

  val remotes = List(remote1, remote2, remote3, remote4, remote5)

//  for(remote <- remotes)
//    tcpConnectionManager ! TcpConnectionManager.CreateOutgoingConnection(remote)

  def receive: Receive = {
    case m =>
      println(m)
  }
}
