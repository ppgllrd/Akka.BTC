/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package network.btc

import akka.actor.{Actor, Props}
import network.message._
import network.tcp.TcpConnection
import util.{Random, UnixTime}

object BtcIncomingConnection {
  def props(btcNode: BtcNode, tcpConnection: TcpConnection) =
    Props(classOf[BtcIncomingConnection], btcNode, tcpConnection)
}
case class BtcIncomingConnection(btcNode: BtcNode, tcpConnection: TcpConnection) extends Actor {
  def receive: Receive = {
    case versionIn: Version =>
      // start handshake
      val versionOut = Version(BtcNode.version, BtcNode.services, UnixTime.now
        , NetworkAddress(0, versionIn.addrFrom.services, tcpConnection.remote)
        , NetworkAddress(0, BtcNode.services, tcpConnection.local)
        , Random.nonce, BtcNode.userAgent, 0, false
      )
      tcpConnection.self ! versionOut

      context become {
        case Verack =>
          // complete handshake
          tcpConnection.self ! Verack
          // handshake has been completed

          // behaviour after handshake
          context become {
            case ping@Ping(nonce) =>
              println(s"Got $ping")
              tcpConnection.self ! Pong(nonce)

            case addr@Addr(count, addrList) =>
              println(s"Got $addr")
              for(networkAddress <- addrList)
                btcNode.networkAddresses ! NetworkAddresses.Add(networkAddress)

            case Malformed(rawMessage, ccode) =>
              println(s"Got a malformed message: $ccode")

            case other =>
              println(s"Got $other")
          }
      }
  }
}
