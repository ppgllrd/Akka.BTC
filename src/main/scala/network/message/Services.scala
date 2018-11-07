/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package network.message

object Services {
  val NODE_NETWORK = 1L
  val NODE_GETUTXO = 2L
  val NODE_BLOOM = 4L
  val NODE_WITNESS = 8L
  val NODE_NETWORK_LIMITED = 1024L
}