/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package network.btc

import akka.actor.{Actor, ActorSystem, Props}
import network.message._
import network.tcp.TcpConnection
import util.{Random, UnixTime}

object BtcIncomingConnection {
  def props(tcpConnection: TcpConnection, actorSystem: ActorSystem) =
    Props(classOf[BtcIncomingConnection], tcpConnection, actorSystem)
}
case class BtcIncomingConnection(tcpConnection: TcpConnection, actorSystem: ActorSystem) extends Actor {
  private val tcpConnectionActor = tcpConnection.self

  // start handshake
  val Some(local) = tcpConnection.local
  val Some(remote) = tcpConnection.remote

  def receive: Receive = {
    case version: Version =>
      // start handshake
      val version2 = Version(BtcNode.version, BtcNode.services, UnixTime.now, NetworkAddress(0, version.addrFrom.services, remote), NetworkAddress(0, BtcNode.services, local), Random.nonce, BtcNode.userAgent, 0, false)
      tcpConnectionActor ! version2

      context become {
        case Verack =>
          // complete handshake
          tcpConnectionActor ! Verack

          context become {
            case Ping(nonce) =>
              tcpConnectionActor ! Pong(nonce)

            case other =>
              println("Got :"+other)
          }
      }
  }
}
