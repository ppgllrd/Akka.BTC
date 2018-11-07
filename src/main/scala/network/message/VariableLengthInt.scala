/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package network.message

import bytes._

object VariableLengthInt {
  def fromBytes(bs : ByteString) : (VariableLengthInt, ByteString) = {
    val prefix : Int = bs(0) & 0xFF

    val (prefixLength, nBytes) =
      if (prefix < 0xFD)
        (0, 1)
      else if (prefix == 0xFD)
        (1, 2)
      else if (prefix == 0xFE)
        (1, 4)
      else if (prefix == 0xFF)
        (1, 8)
      else
        sys.error("VariableLengthInt.fromBytes")

    val (bs1, bs2) = bs.drop(prefixLength).splitAt(nBytes)

    val bigInt = BigInt(bs1.reverse.toArray)
    (VariableLengthInt(bigInt), bs2)
  }
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