/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package network.message

import bytes._
import network.btc.BtcNode
import util.hash.SHA256
import util.{Random, UnixTime}

trait Message {
  val command : String
  val payload : ByteString = ByteString()

  def header : Header = {
    val payloadLength = payload.length

    val checksum =
      SHA256.doubleSha2564Bytes(payload)

    Header(BtcNode.magic, command, payloadLength, checksum)
  }

  def toRawMessage: RawMessage =
    RawMessage(header, payload)

  def toBytes : ByteString =
    toRawMessage.toBytes
}


object Message {
  def fromRawMessage(rawMessage : RawMessage) : Either[Malformed,Message] = {

    // verify checksum
    if (rawMessage.header.checksum != SHA256.doubleSha2564Bytes(rawMessage.payload))
      Left(Malformed(rawMessage, CCodes.REJECT_MALFORMED))
    else {
      // check all trailing bytes in command are 0
      val (command, remaining) = rawMessage.header.command.span(_ != 0)
      if (remaining.exists(_ != 0))
        Left(Malformed(rawMessage, CCodes.REJECT_MALFORMED))
      else {
        val message = command match {
          case "version" =>
            // todo I see state transformer monads all around me
            val (version, bs1) = FromBytes.int(rawMessage.payload, 4)
            val (services, bs2) = FromBytes.long(bs1, 8)
            val (timestamp, bs3) = FromBytes.long(bs2, 8)
            val (addrRecv, bs4) = NetworkAddress.fromBytes(bs3, false)
            val (addrFrom, bs5) = NetworkAddress.fromBytes(bs4, false)
            val (nonce, bs6) = FromBytes.long(bs5, 8)
            val (userAgent, bs7) = VariableLengthString.fromBytes(bs6)
            val (height, bs8) = FromBytes.int(bs7, 4)
            val (relay, bs9) = if (bs8.nonEmpty) FromBytes.bool(bs8) else (false, bs8)

            Version(version, services, timestamp, addrRecv, addrFrom, nonce, userAgent.string, height, relay)

          case "verack" =>
            Verack

          case "ping" =>
            Ping()

          case "pong" =>
            val nonce = FromBytes.long().run(rawMessage.payload)
            Pong(nonce)

          case "addr" =>
            val (count, bs1) = VariableLengthInt.fromBytes(rawMessage.payload)

            var bs = bs1
            var addrs = List[NetworkAddress]()
            for (i <- BigInt(0) until count.value) {
              val (addr, bs2) = NetworkAddress.fromBytes(bs)
              addrs ::= addr
              bs = bs2
            }

            Addr(count.value, addrs.reverse)

          case "reject" =>
            val (message, bs1) = VariableLengthString.fromBytes(rawMessage.payload)
            val (ccode, bs2) = FromBytes.byte(bs1)
            val (reason, bs3) = VariableLengthString.fromBytes(bs2)
            val data = bs3

            Reject(message.string, ccode, reason.string, data)

          case command =>
            Unsupported(command)
        }
        Right(message)
      }
    }
  }
}


case class Addr(count : BigInt, addrList : Seq[NetworkAddress]) extends Message {
  override val command = "addr"
  override val payload = {
    VariableLengthInt(count).toBytes ++ addrList.flatMap(_.toBytes())
  }
}


case class Version(version : Int, services : Long, timestamp : Long, addrRecv : NetworkAddress, addrFrom : NetworkAddress, nonce : Long, userAgent : String, startHeight : Int, relay : Boolean) extends Message {
  override val command = "version"

  override val payload = {
    val versionBs = ToBytes.fromInt(version, 4)
    val servicesBs = ToBytes.fromLong(services, 8)
    val timestampBs = ToBytes.fromLong(timestamp, 8)
    val addrRecvBs = addrRecv.toBytes(false) // NetworkAddress(0, Constants.services, addrRecv.getAddress, addrRecv.getPort).toByteString(false)
    val addrFromBs = addrFrom.toBytes(false) // NetworkAddress(0, Constants.services, addrFrom.getAddress, addrFrom.getPort).toByteString(false)
    val nonceBs = ToBytes.fromLong(nonce, 8)
    val userAgentBs = VariableLengthString(userAgent).toBytes
    val startHeightBs = ToBytes.fromInt(startHeight, 4)
    val relayBs = ToBytes.fromBoolean(relay, 1)

    versionBs ++ servicesBs ++ timestampBs ++
      addrRecvBs ++ addrFromBs ++ nonceBs ++
      userAgentBs ++ startHeightBs ++ relayBs
  }

  override def toString: String =
    s"Version($version,$services,${UnixTime.toString(timestamp)},${addrRecv.toString(false)},${addrFrom.toString(false)},$nonce,$userAgent,$startHeight,$relay)"
}


case object Verack extends Message {
  override val command = "verack"
}


case object Getaddr extends Message {
  override val command = "getaddr"
}


object Ping {
  def apply() : Ping =
    new Ping(Random.nonce)
}

case class Ping(nonce : Long) extends Message {
  override val command = "ping"
  override val payload = {
    ToBytes.fromLong(nonce, 8)
  }
}


case class Pong(nonce : Long) extends Message {
  override val command = "pong"
  override val payload = {
    ToBytes.fromLong(nonce, 8)
  }
}


case class Reject(message : String, ccode : Byte, reason : String, data : ByteString) extends Message {
  override val command = "reject"
  override val payload = {
    VariableLengthString(message).toBytes ++
    ToBytes.fromByte(ccode) ++
    VariableLengthString(reason).toBytes ++
    data
  }
}


// These are not real BTC protocol messages. They are used for notifying parse errors

case class Unsupported(msg : String) extends Message {
  override val command = "Unsupported "+msg
}
