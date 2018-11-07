/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package bytes

object ToBytes {
  def fromInt(x : Int, nBytes : Int = 4) : ByteString = {
    val bs = new Array[Byte](nBytes)
    var n = x
    for(i <- 0 until nBytes) {
      bs(i) = (n & 0xFF).toByte
      n = n >> 8
    }
    ByteString.fromArrayUnsafe(bs)
  }

  def fromLong(x : Long, nBytes : Int = 8) : ByteString = {
    val bs = new Array[Byte](nBytes)
    var n = x
    for(i <- 0 until nBytes) {
      bs(i) = (n & 0xFF).toByte
      n = n >> 8
    }
    ByteString.fromArrayUnsafe(bs)
  }

  def fromBigInt(x : BigInt, nBytes : Int = 4) : ByteString = {
    val bs = new Array[Byte](nBytes)
    var n = x
    for(i <- 0 until nBytes) {
      bs(i) = (n & 0xFF).toByte
      n = n >> 8
    }
    ByteString.fromArrayUnsafe(bs)
  }

  def fromString(xs : String, nBytes : Int) : ByteString = {
    val bs = new Array[Byte](nBytes)  // filled up with 0x00
    for(i <- 0 until xs.length)
      bs(i) = xs(i).toByte
    ByteString.fromArrayUnsafe(bs)
  }

  def fromString(xs : String) : ByteString =
    fromString(xs, xs.length)

  def fromBoolean(b : Boolean, nBytes : Int = 1) : ByteString =
    fromInt(if(b) 1 else 0, nBytes)
}