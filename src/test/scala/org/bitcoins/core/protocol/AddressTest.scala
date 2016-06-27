package org.bitcoins.core.protocol

import org.bitcoins.core.util.{BitcoinSLogger, TestUtil}
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 3/23/15.
 */
class AddressTest extends FlatSpec with MustMatchers with BitcoinSLogger {
  val assetAddress = TestUtil.assetAddress
  "Addresses" must "be able to convert back and forth between a Bitcoin Address & an asset address" in {
    val convertedOnce = BitcoinAddress.convertToAssetAddress(TestUtil.bitcoinAddress)
    val actual : BitcoinAddress = AssetAddress.convertToBitcoinAddress(convertedOnce)
    actual must be (TestUtil.bitcoinAddress)
    val bitcoinAddress = AssetAddress.convertToBitcoinAddress(assetAddress)
    val actualAssetAddress = BitcoinAddress.convertToAssetAddress(bitcoinAddress)
    actualAssetAddress must be (assetAddress)
  }

  it must "allow type encapsulation for addresses" in {

    val bitcoinAddress : Address = TestUtil.bitcoinAddress
    val assetAddress : Address = TestUtil.assetAddress
    assetAddress must be (TestUtil.assetAddress)
    bitcoinAddress must be (TestUtil.bitcoinAddress)
  }

  it must "throw an exception for an invalid address" in {
    intercept[RuntimeException] {
      Address("2N2JD6wb56AfK4tfmM6PwdVmoYk2dCKf4Br222")
    }
  }
}
