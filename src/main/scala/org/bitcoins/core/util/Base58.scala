package org.bitcoins.core.util

import scala.annotation.tailrec
import scala.util.{Failure, Success}

/**
  * Created by chris on 5/16/16.
  * source of values: https://en.bitcoin.it/wiki/Base58Check_encoding
  */
trait Base58 extends BitcoinSLogger {

  val base58Characters = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  val base58Pairs = base58Characters.zipWithIndex.toMap


  def decodeCheck(input: String) : Seq[Byte] = {
    val decoded : Seq[Byte] = decode(input)
    if (decoded.length < 4) throw new RuntimeException("Invalid input")
    else {
      val splitSeqs = decoded.splitAt(decoded.length - 4)
      val data : Seq[Byte] = splitSeqs._1
      val checksum : Seq[Byte] = splitSeqs._2
      val actualChecksum : Seq[Byte] = CryptoUtil.doubleSHA256(data).slice(0, 4)
      if (checksum == actualChecksum)
      data
      else throw new IllegalArgumentException("checksums don't validate")
    }
  }


  /**
    * Takes in sequence of bytes and returns base58 bitcoin address
    * Used ACINQ's implementation as reference under Apache License
    * Modified to use Scala's BigInt rather than Java's BigInteger
    * https://github.com/ACINQ/bitcoin-lib/blob/master/src/main/scala/fr/acinq/bitcoin/Base58.scala
    *
    * @param bytes
    * @return
    */
  def encode(bytes : Seq[Byte]) : String = {
    if (bytes.isEmpty) ""
    else {
      val big : BigInt = BigInt(1, bytes.toArray)
      val builder = new StringBuilder

      @tailrec
      def loop(current : BigInt) : String = current match {
        case a if current == BigInt(0) => ""
        case _ =>
          val quotient : BigInt = current / BigInt(58L)
          val remainder  = current.mod(58L)
          builder.append(base58Characters.charAt(remainder.intValue())) //
          loop(quotient)
      }
      loop(big)
      bytes.takeWhile(_ == 0).map(_ => builder.append(base58Characters.charAt(0)))
      builder.toString().reverse
    }
  }

  /**
    * Takes in base58 string and returns sequence of bytes
    * https://github.com/ACINQ/bitcoin-lib/blob/master/src/main/scala/fr/acinq/bitcoin/Base58.scala
    *
    * @param input
    * @return
    */
  def decode(input: String) : Seq[Byte] = {
    val zeroes = input.takeWhile(_ == '1').map(_ => 0:Byte).toArray
    val trim  = input.dropWhile(_ == '1').toList
    val decoded = trim.foldLeft(BigInt(0))((a,b) =>a.*(BigInt(58L)).+(BigInt(base58Pairs(b))))
    if (trim.isEmpty) zeroes else zeroes ++ decoded.toByteArray.dropWhile(_ == 0).toList.toSeq // BigInteger.toByteArray may add a leading 0x00
  }

}

object Base58 extends Base58



