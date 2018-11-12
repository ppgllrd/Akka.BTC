/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package network.message

import bytes._

object RawMessage {
  def parserOpt: Parser[Option[RawMessage]] =
    Header.parserOpt.flatMap {
      case None =>
        Parser.pure(None)
      case Some(header) =>
        Parser.input.flatMap { bs =>
          if (bs.length < header.payloadLength)
            Parser.pure(None)
          else
            for {
              payload <- Parser.take(header.payloadLength.toInt)
              rawMsg = RawMessage(header, payload)
            } yield Some(rawMsg)
        }
    }
}


case class RawMessage(header : Header, payload : ByteString) {
  def toBytes : ByteString =
    header.toBytes ++ payload
}