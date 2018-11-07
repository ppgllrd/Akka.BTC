/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package util

object Random {
  def nonce : Long =
    scala.util.Random.nextLong()
}
