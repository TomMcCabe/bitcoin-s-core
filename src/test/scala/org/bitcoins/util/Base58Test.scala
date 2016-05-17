package org.bitcoins.util

import org.scalatest.{MustMatchers, FlatSpec}

/**
  * Created by tom on 5/17/16.
  */
class Base58Test extends FlatSpec with MustMatchers {
  "Base58" must "encode byte value of 0 to character of 1" in {
    Base58.encode(0.toByte) must be ('1')
  }

  it must "encode byte value of 1 to character 2" in {
    Base58.encode(1.toByte) must be ('2')
  }

  it must "decode character 1 to byte value of 0" in {
    Base58.decode('1') must be (0.toByte)
  }

  it must "encode all valid Base58 bytes to corresponding character, then decode back to original byte" in {
    val validBase58Bytes : Seq[Byte] = for {
      i<-0 to 57
    } yield i.toByte

    val encodingCheck : Seq[Boolean] = validBase58Bytes.map(
      byte => Base58.decode(Base58.encode(byte)) == byte
    )

    encodingCheck.exists(_ == false) must be (false)

  }
}
