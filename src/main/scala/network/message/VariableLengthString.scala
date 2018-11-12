/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package network.message

import bytes._

object VariableLengthString {
  def parser : Parser[VariableLengthString] =
    for {
      length <- VariableLengthInt.parser
      bs <- Parser.take(length.value.toInt)
      string = bs.decodeString(java.nio.charset.StandardCharsets.US_ASCII)
    } yield VariableLengthString(string)
}


case class VariableLengthString(string : String) {
  def toBytes : ByteString =
    VariableLengthInt(string.length).toBytes ++ ToBytes.fromString(string)
}