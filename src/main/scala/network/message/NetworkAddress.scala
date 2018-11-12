/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package network.message

import java.net.{InetAddress, InetSocketAddress}

import bytes._
import util.UnixTime

object NetworkAddress {
  def apply(time: Long, services: Long, inetSocketAddress: InetSocketAddress): NetworkAddress =
    NetworkAddress(time, services, InetAddress.getByAddress(inetSocketAddress.getAddress.getAddress), inetSocketAddress.getPort)

  val ip4prefix: ByteString = ByteString.fromArrayUnsafe(Array.fill[Byte](10)(0) ++ Array.fill[Byte](2)(0xFF.toByte))

  def parser(includeTime: Boolean = true) : Parser[NetworkAddress] =
    for {
      time <- if (includeTime) Parser.long(4) else Parser.pure(0L)
      services <- Parser.long(8)
      inetAddressBytes <- Parser.take(16)
      inetAddress =
        if (inetAddressBytes.startsWith(ip4prefix))
          InetAddress.getByAddress(inetAddressBytes.drop(ip4prefix.length).toArray)
        else
          InetAddress.getByAddress(inetAddressBytes.toArray)
      port <- Parser.LittleEndian.int(2)
    } yield NetworkAddress(time, services, inetAddress, port)
}


case class NetworkAddress(time : Long, services : Long, inetAddress : InetAddress, port : Int) {
  def toBytes(includeTime : Boolean = true) : ByteString = {
    var ip = ByteString.fromArrayUnsafe(inetAddress.getAddress)
    if(ip.length < 16)
      ip = NetworkAddress.ip4prefix ++ ip
    val servicesBs = ToBytes.fromLong(services, 8)
    val portBs = ToBytes.LittleEndian.fromInt(port, 2)
    var bs = servicesBs ++ ip ++ portBs
    if(includeTime)
      bs = ToBytes.fromLong(time, 4) ++ bs
    bs
  }

  def toString(includeTime : Boolean = true): String =
    s"NetworkAddress(${if(includeTime) UnixTime.toString(time)+"," else ""}$services,$inetAddress,$port)"

  override def toString: String =
    toString(true)

  override def equals(o: scala.Any): Boolean = o match {
    case that: NetworkAddress =>
      this.inetAddress.equals(that.inetAddress)
    case _ =>
      false
  }

  override def hashCode(): Int =
    inetAddress.hashCode()

}