/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package network.message

import bytes._

object VariableLengthString {
  def fromBytes(bs : ByteString) : (VariableLengthString, ByteString) = {
    val (length, bs1) = VariableLengthInt.fromBytes(bs)
    val (bs2, bs3) = bs1.splitAt(length.value.toInt)
    val string = bs2.decodeString(java.nio.charset.StandardCharsets.US_ASCII)

    (VariableLengthString(string), bs3)
  }
}


case class VariableLengthString(string : String) {
  def toBytes : ByteString =
    VariableLengthInt(string.length).toBytes ++ ToBytes.fromString(string)
}