/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package network.message

import bytes._

object VariableLengthInt {
  def parser : Parser[VariableLengthInt] =
    for {
      byte <- Parser.byte
      mask : Int = byte & 0xFF
      (prepend, nBytes) =
        if (mask < 0xFD)
          (true, 0)
        else if (mask == 0xFD)
          (false, 2)
        else if (mask == 0xFE)
          (false, 4)
        else if (mask == 0xFF)
          (false, 8)
        else
          sys.error("VariableLengthInt.fromBytes")
      bs1 <- Parser.take(nBytes)
      bs2 = if (prepend) byte +: bs1 else bs1
      bigInt = BigInt(bs2.reverse.toArray)
    } yield VariableLengthInt(bigInt)
}

case class VariableLengthInt(value : BigInt) {
  private val bytes : Array[Byte] =
    if(value < 0xFD)
      Array(value.toByte)
    else if(value <= 0xFFFF)
      Array(0xFD.toByte) ++ ToBytes.fromBigInt(value, 2)
    else if(value <= 0xFFFFFFFF)
      Array(0xFE.toByte) ++ ToBytes.fromBigInt(value, 4)
    else
      Array(0xFF.toByte) ++ ToBytes.fromBigInt(value, 8)

  val toBytes : ByteString =
    ByteString.fromArrayUnsafe(bytes)
}