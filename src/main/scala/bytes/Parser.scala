/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package bytes

import util.monad.StateTransformer

object Parser {
  private def bytesToBigInt(bs : ByteString) : BigInt = {
    var x = BigInt(0)
    for(b <- bs) {
      val int = b & 0xFF
      x = x*256 + int
    }
    x
  }

  type Parser[A] = StateTransformer[ByteString, A]

  def apply[A](fst : ByteString => (ByteString, A)) =
    StateTransformer(fst)

  def pure[A](x : A) : Parser[A] =
    StateTransformer.pure(x)

  def input : Parser[ByteString] =
    StateTransformer.read

  object LittleEndian {
    def int(nBytes : Int = 4) : Parser[Int] = Parser{ bs =>
      val (bs1, bs2) = bs.splitAt(nBytes)
      val int = bytesToBigInt(bs1).toInt
      (bs2, int)
    }
  }

  // All of these use Big Endian encoding
  def long(nBytes : Int = 8) : Parser[Long] = Parser{ bs =>
    val (bs1, bs2) = bs.splitAt(nBytes)
    val long = bytesToBigInt(bs1.reverse).toLong
    (bs2, long)
  }

  def int(nBytes : Int = 4) : Parser[Int] = Parser{ bs =>
    val (bs1, bs2) = bs.splitAt(nBytes)
    val int = bytesToBigInt(bs1.reverse).toInt
    (bs2, int)
  }

  def byte : Parser[Byte] = Parser{ bs =>
    (bs.tail, bs.head)
  }

  def bool : Parser[Boolean] = Parser{ bs =>
    (bs.tail, bs(0) == 1)
  }

  def take(n : Int) : Parser[ByteString] = Parser{ bs =>
    bs.splitAt(n).swap
  }
}
