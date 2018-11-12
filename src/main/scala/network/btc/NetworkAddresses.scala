/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package network.btc

import java.net.InetSocketAddress

import akka.actor.{Actor, Props}
import network.btc.NetworkAddresses.{ConnectedTo, NewCandidate}
import network.message.{NetworkAddress, Version}
import network.tcp.TcpConnectionManager

object NetworkAddresses {
  def props(btcNode: BtcNode) =
    Props(classOf[NetworkAddresses], btcNode)
  case class NewCandidate(networkAddress: NetworkAddress)

  case class ConnectedTo(version: Version)
}

case class NetworkAddresses(btcNode: BtcNode) extends Actor {
  private val candidates = scala.collection.mutable.Set[NetworkAddress]()

  private val recentlyContacted = scala.collection.mutable.Set[Version]()

  def print(): Unit = {
    val allAddresses = candidates.toList.sortBy(- _.time)

    if(candidates.size % 5000 == 0) {
      val ps = new java.io.PrintStream(s"nodes${candidates.size}.txt")
      for (address <- allAddresses)
        ps.println(address)
      ps.close()
    }


    def choose(maxIndex : Int) = {
      var choosen : Option[NetworkAddress] = None

      while(choosen.isEmpty) {
        val tentative = allAddresses(scala.util.Random.nextInt(maxIndex))
        if(!recentlyContacted.map(_.addrFrom.inetAddress).contains(tentative.inetAddress))
          choosen = Some(tentative)
      }
      choosen.get
    }

    for(i <- 0 until 5) {
      val networkAddresses = choose(allAddresses.length/3)
      val inetSocketAddress = InetSocketAddress.createUnresolved(networkAddresses.inetAddress.getHostAddress, networkAddresses.port)
      btcNode.tcpConnectionManager ! TcpConnectionManager.CreateOutgoingConnection(inetSocketAddress)
    }

    for(i <- 0 until 1) {
      val networkAddresses = choose(allAddresses.length)
      val inetSocketAddress = InetSocketAddress.createUnresolved(networkAddresses.inetAddress.getHostAddress, networkAddresses.port)
      btcNode.tcpConnectionManager ! TcpConnectionManager.CreateOutgoingConnection(inetSocketAddress)
    }
  }

  override def receive: Receive = {
    case NewCandidate(networkAddress) =>
      candidates += networkAddress
      if(candidates.size % 100 == 0)
        print()

    case ConnectedTo(version) =>
      recentlyContacted.add(version)
      if(recentlyContacted.size % 100 == 0) {
        val ps = new java.io.PrintStream(s"connections${recentlyContacted.size}.txt")
        for (version <- recentlyContacted)
          ps.println(version)
        ps.close()
      }
  }
}
