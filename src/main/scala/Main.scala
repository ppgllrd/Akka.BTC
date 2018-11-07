/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

import java.net.{InetAddress, InetSocketAddress}

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import network.message._
import network.node.{Client, Node, SendMessage}
import util.{Random, UnixTime}
import scala.concurrent.duration._


object Main extends App {
  private implicit val actorSystem = ActorSystem()
  private implicit val materializer = ActorMaterializer()
  implicit val executionContext = actorSystem.dispatcher

  val remote1 = new InetSocketAddress("54.36.61.219", 8333)
  val remote2 = new InetSocketAddress("82.74.137.168", 8333)
  val remote3 = new InetSocketAddress("183.193.213.254", 8333)
  val remote4 = new InetSocketAddress("34.237.15.134", 8333)


  val scheduler = actorSystem.scheduler


  // val node = actorSystem.actorOf(ClientManager.props(actorSystem))

  def newListener(inetSocketAddress: InetSocketAddress): Unit = {
    //node ! inetSocketAddress
    val node = actorSystem.actorOf(Client.props(inetSocketAddress, actorSystem))

    scheduler.scheduleOnce(3 seconds) {
      val na = NetworkAddress(0, Node.services, InetAddress.getByAddress(inetSocketAddress.getAddress.getAddress), inetSocketAddress.getPort)
      // todo use my ip
      val version = Version(Node.version, Node.services, UnixTime.now, na, na, Random.nonce, Node.userAgent, 0, false)
      node ! SendMessage(version)
      node ! SendMessage(Verack)

      scheduler.scheduleOnce(3 seconds) {
        node ! SendMessage(Ping())

        scheduler.scheduleOnce(2 seconds) {
          node ! SendMessage(Ping())

          scheduler.scheduleOnce(2 seconds) {
            node ! SendMessage(Getaddr)
          }
        }
      }
    }
  }


  for(n <- List(remote1, remote2, remote3, remote4))
    newListener(n)
}