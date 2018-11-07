/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package bytes

object FromBytes {
  private def bytesToBigInt(bs : ByteString) : BigInt = {
    var x = BigInt(0)
    for(b <- bs) {
      val int = b & 0xFF
      x = x*256 + int
    }
    x
  }

  object LittleEndian {
    def int(bs : ByteString, nBytes : Int = 4) : (Int, ByteString) = {
      val (bs1, bs2) = bs.splitAt(nBytes)
      val int = bytesToBigInt(bs1).toInt
      (int, bs2)
    }
  }

  // All of these use Big Endian encoding
  def long(bs : ByteString, nBytes : Int = 8) : (Long, ByteString) = {
    val (bs1, bs2) = bs.splitAt(nBytes)
    val long = bytesToBigInt(bs1.reverse).toLong
    (long, bs2)
  }

  def int(bs : ByteString, nBytes : Int = 4) : (Int, ByteString) = {
    val (bs1, bs2) = bs.splitAt(nBytes)
    val int = bytesToBigInt(bs1.reverse).toInt
    (int, bs2)
  }

  def bool(bs : ByteString) : (Boolean, ByteString) =
    (bs(0)==1, bs.tail)
}
