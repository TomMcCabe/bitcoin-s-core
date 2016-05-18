package org.bitcoins.util

import org.bitcoins.util
import org.scalatest.{MustMatchers, FlatSpec}

/**
  * Created by tom on 5/17/16.
  */
class Base58Test extends FlatSpec with MustMatchers {
  "Base58" must "encode byte value of 0 to character of 1" in {
    Base58.encode(0.toByte) must be ('1')
  }

  it must "encode byte value of 22 to character P" in {
    Base58.encode(22.toByte) must be ('P')
  }

  it must "decode character 1 to byte value of 0" in {
    Base58.decode('1') must be (0.toByte)
  }

  it must "decode character Z to byte value of 32" in {
    Base58.decode('Z') must be (32.toByte)
  }

  it must "encode all valid Base58 bytes to corresponding character, then decode back to original byte" in {
    val encodingCheck : Seq[Boolean] = Base58.validBase58Bytes.map(
      byte => Base58.decode(Base58.encode(byte)) == byte
    )
    encodingCheck.contains(false) must be (false)
  }

  it must "decode sequence of characters" in {
    val seqChars = Seq('a','5','Z','g')
    Base58.decode(seqChars) must be (Seq(33, 4, 32, 39).map(x=>x.toByte))
  }

  it must "encode sequence of bytes and return character string" in {
    val seqBytes = Seq(34, 56, 51, 37).map(x=>x.toByte)
    Base58.encode(seqBytes) must be ("byte")
  }

  it must "decode base58 string and return seq of bytes" in {
    val address = "1BvBMSEYstWetqTFn5Au4m4GFg7xJaNVN2"
    Base58.decode(address) must be (List(0, 10, 53, 10, 20, 25, 13, 31, 50, 51, 29, 37, 51, 48, 26,
      14, 45, 4, 9, 52, 3, 44, 3, 15, 14, 39, 6, 55, 17, 33, 21, 28, 21, 1))
  }

  it must "encode a sequence of bytes into a base58 string and decode it to original value" in {
    val list : Seq[Byte] = List(1,2,3)
    val encodedList : String = Base58.encode(list)
    val decodedList : Seq[Byte] = Base58.decode(encodedList)

    decodedList must be (Base58.decode(Base58.encode(List(1,2,3).map(x=>x.toByte))))
  }

}
