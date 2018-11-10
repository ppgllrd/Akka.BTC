/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package network.tcp

import java.net.InetSocketAddress

import akka.actor.ActorRef
import akka.util.ByteString

object TcpConnection {
  case class SendBytes(bs : ByteString)
}

case class TcpConnection(id : Int, self : ActorRef, local : InetSocketAddress, remote: InetSocketAddress)

