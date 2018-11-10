/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package util.monad

case class StateTransformer[S,+A](fst : S => (S, A)) {
  def map[B](f : A => B) : StateTransformer[S,B] =
    StateTransformer((st : S) => {
      val (st1, x) = this.fst(st)
      (st1, f(x))
    })

  def flatMap[B](f : A => StateTransformer[S,B]) : StateTransformer[S,B] =
    StateTransformer((st : S) => {
      val (st1, x) = this.fst(st)
      val stt = f(x)
      stt.fst(st1)
    })

  def apply(st : S) : (S, A) = this.fst(st)

  def run(st : S) : A = this.fst(st)._2

  def >>=[B](f : A => StateTransformer[S,B]) : StateTransformer[S,B] = this.flatMap(f)

  def >>[B](stt : StateTransformer[S,B]) : StateTransformer[S,B] = this.flatMap(_ => stt)
}

object StateTransformer {
  def pure[S,A](x : A) : StateTransformer[S,A] = StateTransformer(st => (st, x))

  def read[S] : StateTransformer[S,S] = StateTransformer(st => (st, st))

  def write[S](st : S) : StateTransformer[S,Unit] = StateTransformer(_ => (st, Unit))
}