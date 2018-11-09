/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package util.hash

import java.security.MessageDigest

import bytes.ByteString

object SHA256 {

  // We cannot reuse digest as code must be reentrant
  def sha256(bs : Array[Byte]) : Array[Byte] = {
    val sha256Digest = MessageDigest.getInstance("SHA-256")
    sha256Digest.digest(bs)
  }

  def doubleSha256(bs : Array[Byte]) : Array[Byte] = {
    val sha256Digest = MessageDigest.getInstance("SHA-256")
    val xs = sha256Digest.digest(bs)
    val ys = sha256Digest.digest(xs)
    ys
  }

  def sha256(bs : ByteString) : ByteString = {
    val sha256Digest = MessageDigest.getInstance("SHA-256")
    sha256Digest.update(bs.asByteBuffer)
    val xs = sha256Digest.digest()
    ByteString.fromArrayUnsafe(xs)
  }

  def doubleSha256(bs : ByteString) : ByteString = {
    val sha256Digest = MessageDigest.getInstance("SHA-256")
    sha256Digest.update(bs.asByteBuffer)
    val xs = sha256Digest.digest()
    val ys = sha256Digest.digest(xs)

    ByteString.fromArrayUnsafe(ys)
  }

  def doubleSha2564Bytes(bs : ByteString) : ByteString = {
    val sha256Digest = MessageDigest.getInstance("SHA-256")
    sha256Digest.update(bs.asByteBuffer)
    val xs = sha256Digest.digest()
    val ys = sha256Digest.digest(xs)

    ByteString.fromArrayUnsafe(ys, 0, 4)
  }
}
