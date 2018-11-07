/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

import akka.actor.ActorSystem
import network.btc.BtcNode

object Main extends App {
  private implicit val actorSystem = ActorSystem()

  private val btcNode = actorSystem.actorOf(BtcNode.props(actorSystem))
}