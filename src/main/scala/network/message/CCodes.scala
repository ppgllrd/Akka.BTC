/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package network.message

object CCodes {
  val REJECT_MALFORMED : Byte = 0x01
  val REJECT_INVALID : Byte = 0x10
  val REJECT_OBSOLETE : Byte = 0x11
  val REJECT_DUPLICATE : Byte = 0x12
  val REJECT_NONSTANDARD : Byte = 0x40
  val REJECT_DUST : Byte = 0x41
  val REJECT_INSUFFICIENTFEE : Byte = 0x42
  val REJECT_CHECKPOINT : Byte = 0x43
}
