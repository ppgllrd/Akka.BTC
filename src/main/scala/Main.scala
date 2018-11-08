/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

import akka.actor.ActorSystem
import network.btc.BtcNodeActor

object Main extends App {
  private implicit val actorSystem = ActorSystem()

  // create root actor
  val btcNodeActor = actorSystem.actorOf(BtcNodeActor.props(actorSystem))
}