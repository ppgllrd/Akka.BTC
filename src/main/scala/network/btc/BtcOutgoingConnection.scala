/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package network.btc

import java.net.InetAddress

import akka.actor.{Actor, Props}
import network.message._
import network.tcp.TcpConnection
import util.{Random, UnixTime}

object BtcOutgoingConnection {
  def props(btcNode: BtcNode, tcpConnection: TcpConnection) =
    Props(classOf[BtcOutgoingConnection], btcNode, tcpConnection)
}

case class BtcOutgoingConnection(btcNode: BtcNode, tcpConnection: TcpConnection) extends Actor {
  val actorSystem = btcNode.actorSystem

  // start handshake
  private val versionOut =
    Version(BtcNode.version, BtcNode.services, UnixTime.now
      , NetworkAddress(0, 0, tcpConnection.remote)
      , NetworkAddress(0, BtcNode.services, tcpConnection.local)
      , Random.nonce
      , BtcNode.userAgent, 0, false
    )
  tcpConnection.self ! versionOut

  def receive: Receive = {
    case versionIn: Version =>
      // complete handshake
      tcpConnection.self ! Verack

      context become {
        case Verack =>
          // handshake has been completed

          // send our server address
          val myAddress = InetAddress.getByAddress(versionIn.addrRecv.inetAddress.getAddress)
          val myPort = BtcNode.tcpServerAddress.getPort
          val myServices = BtcNode.services
          tcpConnection.self ! Addr(1, List(NetworkAddress(UnixTime.now, myServices, myAddress, myPort)))

          // ask peer for known addresses
          (1 to 10).foreach( _ =>
            tcpConnection.self ! Getaddr
          )

          context become {
            case ping@Ping(nonce) =>
              println(s"Got $ping")
              tcpConnection.self ! Pong(nonce)

            case addr@Addr(count, addrList) =>
              println(s"Got $addr")
              for (networkAddress <- addrList)
                btcNode.networkAddresses ! NetworkAddresses.Add(networkAddress)

            case Malformed(rawMessage, ccode) =>
              println(s"Got a malformed message: $ccode")

            case other =>
              println(s"Got $other")
          }
      }
  }
}
