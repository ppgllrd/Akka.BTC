/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package network.message

import bytes._

object RawMessage {
  def fromBytesOpt(bs : ByteString) : Option[(RawMessage, ByteString)] = {
    Header.fromBytesOpt(bs) match {
      case None =>
        None
      case Some((header, bs1)) =>
        if(bs1.length < header.payloadLength)
          None
        else {
          val (payload, bs2) = bs1.splitAt(header.payloadLength.toInt)
          // todo verify checksum
          val msg = RawMessage(header, payload)

          Some((msg, bs2))
        }
    }
  }
}


case class RawMessage(header : Header, payload : ByteString) {
  def toBytes : ByteString =
    header.toBytes ++ payload
}