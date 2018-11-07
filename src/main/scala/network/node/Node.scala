/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package network.node

import network.message.{Magic, Services}


object Node {
  val magic : Int = Magic.mainnet
  val services : Long = Services.NODE_NETWORK
  val version : Int = 70005
  val userAgent = "ScalaBTC:0.0.1"
}


class Node {

}
