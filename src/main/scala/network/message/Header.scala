/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package network.message

import bytes._
import network.btc.BtcNode


object Header {
  object Length {
    val magic : Int = 4
    val command : Int = 12
    val length : Int = 4
    val checksum : Int = 4
  }

  private val headerLength = Length.magic + Length.command + Length.length + Length.checksum

  private val magicBytes = ToBytes.fromInt(BtcNode.magic, Length.magic)

  def parserOpt : Parser[Option[Header]] = Parser{ bs =>
    val i = bs.indexOfSlice(magicBytes)

    if (i < 0)
      (bs, None)
    else {
      val bs1 = bs.drop(i)
      if(i>0)
        println(s"Dropped $i bytes") //todo use log and pass this information somehow to BtcConnection

      if (bs1.length < headerLength)
        (bs, None)
      else {
        val headerParser = for {
          magic <- Parser.int(Length.magic)
          commandBytes <- Parser.take(Length.command)
          command = commandBytes.decodeString(java.nio.charset.StandardCharsets.US_ASCII)
          payloadLength <- Parser.long(Length.length)
          checksum <- Parser.take(Length.checksum)
        } yield Some(Header(magic, command, payloadLength, checksum))

        headerParser(bs)
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