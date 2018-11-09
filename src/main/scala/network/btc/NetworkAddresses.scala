/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package network.btc

import java.net.InetSocketAddress

import akka.actor.{Actor, Props}
import network.btc.NetworkAddresses.Add
import network.message.NetworkAddress
import network.tcp.TcpConnectionManager

object NetworkAddresses {
  def props(btcNode: BtcNode) =
    Props(classOf[NetworkAddresses], btcNode)
  case class Add(networkAddress: NetworkAddress)
}

case class NetworkAddresses(btcNode: BtcNode) extends Actor {
  private val addresses = scala.collection.mutable.Set[NetworkAddress]()

  def print(): Unit = {
    val allAddresses = addresses.toList.sortBy(- _.time)

    if(addresses.size % 1000 == 0) {
      val ps = new java.io.PrintStream(s"nodes${addresses.size}.txt")
      for (address <- allAddresses)
        ps.println(address)
      ps.close()
    }

    for(i <- 0 until 10) {
      val node = allAddresses(scala.util.Random.nextInt(allAddresses.length))
      val inetSocketAddress = InetSocketAddress.createUnresolved(node.inetAddress.getHostAddress, node.port)
      btcNode.tcpConnectionManager ! TcpConnectionManager.CreateOutgoingConnection(inetSocketAddress)
    }
  }

  override def receive: Receive = {
    case Add(networkAddress) =>
      addresses.add(networkAddress)
      if(addresses.size % 100 == 0)
        print()
  }
}
