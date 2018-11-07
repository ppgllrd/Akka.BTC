/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package network.btc

import java.net.InetAddress
import akka.actor.{Actor, ActorSystem, Props}
import network.message._
import network.tcp.TcpConnection
import util.{Random, UnixTime}

object BtcOutgoingConnection {
  def props(tcpConnection: TcpConnection, actorSystem: ActorSystem) =
    Props(classOf[BtcOutgoingConnection], tcpConnection, actorSystem)
}

case class BtcOutgoingConnection(tcpConnection: TcpConnection, actorSystem: ActorSystem) extends Actor {
  private val tcpConnectionActor = tcpConnection.self

  // start handshake
  val Some(local) = tcpConnection.local
  val Some(remote) = tcpConnection.remote

  // todo use my ip
  private val version = Version(BtcNode.version, BtcNode.services, UnixTime.now, NetworkAddress(0, 0, remote), NetworkAddress(0, BtcNode.services, local), Random.nonce, BtcNode.userAgent, 0, false)
  tcpConnectionActor ! version

  def receive: Receive = {
    case version: Version =>
      // complete handshake
      tcpConnectionActor ! Verack

      tcpConnectionActor ! Addr(VariableLengthInt(1), List(NetworkAddress(UnixTime.now, BtcNode.services, InetAddress.getByAddress(version.addrRecv.inetAddress.getAddress), 8333)))

      context become {
        case Ping(nonce) =>
          tcpConnectionActor ! Pong(nonce)

        case addr: Addr =>
          println("Got :"+addr)

        case other =>
          println("Got :"+other)
      }
  }
}
