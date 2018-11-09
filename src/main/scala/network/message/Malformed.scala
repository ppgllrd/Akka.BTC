/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package network.message

// This is not a real BTC protocol message so it does not extend Message.
// It represents a malformed received message.
case class Malformed(rawMessage : RawMessage, ccode : Byte)


