/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package bytes

import util.monad.StateTransformer

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
    def int(nBytes : Int = 4) : M[Int] = M{ bs =>
      val (bs1, bs2) = bs.splitAt(nBytes)
      val int = bytesToBigInt(bs1).toInt
      (bs2, int)
    }
  }

  // All of these use Big Endian encoding
  def long(nBytes : Int = 8) : M[Long] = M{ bs =>
    val (bs1, bs2) = bs.splitAt(nBytes)
    val long = bytesToBigInt(bs1.reverse).toLong
    (bs2, long)
  }

  def int(nBytes : Int = 4) : M[Int] = M{ bs =>
    val (bs1, bs2) = bs.splitAt(nBytes)
    val int = bytesToBigInt(bs1.reverse).toInt
    (bs2, int)
  }

  def byte : M[Byte] = M{ bs =>
    (bs.tail, bs.head)
  }

  def bool : M[Boolean] = M{ bs =>
    (bs.tail, bs(0) == 1)
  }

  def take(n : Int) : M[ByteString] = M{ bs =>
    bs.splitAt(n)
  }
}
