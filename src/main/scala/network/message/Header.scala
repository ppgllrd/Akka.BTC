/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package network.message

import bytes._
import network.node.Node


object Header {
  object Length {
    val magic : Int = 4
    val command : Int = 12
    val length : Int = 4
    val checksum : Int = 4
  }

  private val headerLength = Length.magic + Length.command + Length.length + Length.checksum

  private val magicBytes = ToBytes.fromInt(Node.magic, Length.magic)

  def fromBytesOpt(bs : ByteString) : Option[(Header, ByteString)] = {
    val i = bs.indexOfSlice(magicBytes)

    if (i < 0)
      None
    else {
      val bs1 = bs.drop(i)
      if(i>0)
        println(s"Dropped $i bytes") //todo use log

      if (bs1.length < headerLength)
        None
      else {
        val (magic, bs2) = FromBytes.int(bs1, Length.magic)

        val (commandBytes, bs3) = bs2.splitAt(Length.command)
        // todo check all trailing bytes are 0
        val command = commandBytes.decodeString(java.nio.charset.StandardCharsets.US_ASCII).takeWhile(_ != 0)

        val (payloadLength, bs4) = FromBytes.long(bs3, Length.length)

        val (checksum, bs5) = bs4.splitAt(Length.checksum)

        val header = Header(magic, command, payloadLength, checksum)
        Some((header, bs5))
      }
    }
  }
}


case class Header(magic : Int, command : String, payloadLength : Long, checksum : ByteString) {
  def toBytes : ByteString =
    Header.magicBytes ++
      ToBytes.fromString(command, Header.Length.command) ++
      ToBytes.fromLong(payloadLength, Header.Length.length) ++
      checksum
}