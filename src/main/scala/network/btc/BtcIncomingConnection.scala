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
      tcpConnection.conn ! versionOut

      context become {
        case Verack =>
          // complete handshake
          tcpConnection.conn ! Verack

          // behaviour after handshake
          context become {
            case Ping(nonce) =>
              tcpConnection.conn ! Pong(nonce)

            case addr@Addr(count, addrList) =>
              println("Got :"+addr)
              for(networkAddress <- addrList)
                btcNode.networkAddresses ! NetworkAddresses.Add(networkAddress)

            case other =>
              println("Got :"+other)
          }
      }
  }
}
