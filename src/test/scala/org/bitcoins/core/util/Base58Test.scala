package org.bitcoins.core.util

import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by tom on 5/17/16.
  */
class Base58Test extends FlatSpec with MustMatchers {
  "Base58" must "encode byte value of 0 to character of 1" in {
    Base58.base58Characters(0) must be ('1')
  }

  it must "encode byte value of 22 to character P" in {
    Base58.base58Characters(22) must be ('P')
  }

  it must "decode character 1 to byte value of 0" in {
    Base58.base58Pairs('1') must be (0.toByte)
  }

  it must "decode character Z to byte value of 32" in {
    Base58.base58Pairs('Z') must be (32.toByte)
  }

  it must "decode and return same result as bitcoinj" in {
    val address = "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i"
    val bitcoinj = org.bitcoinj.core.Base58.decode(address)
    Base58.decodeBase58(address) must be (bitcoinj)
  }

}
