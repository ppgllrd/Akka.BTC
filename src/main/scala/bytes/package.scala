import util.monad.StateTransformer

/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package object bytes {
  type ByteString = akka.util.ByteString

  val ByteString = akka.util.ByteString

  type M[A] = StateTransformer[ByteString, A]

  object M {
    def apply[A](fst : ByteString => (ByteString, A)) =
      StateTransformer(fst)
  }
}
