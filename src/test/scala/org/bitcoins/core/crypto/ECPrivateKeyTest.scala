package org.bitcoins.core.crypto

import org.bitcoins.core.util.{BitcoinJTestUtil, BitcoinSUtil, CryptoTestUtil}
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 3/7/16.
 */
class ECPrivateKeyTest extends FlatSpec with MustMatchers {

  "ECPrivateKey" must "have the same byte representation as a bitcoinj private key" in {
    val bitcoinjPrivateKey = CryptoTestUtil.bitcoinjPrivateKey.getPrivateKeyAsHex
    CryptoTestUtil.privateKey.hex must be (bitcoinjPrivateKey)
  }

  it must "derive the same public from a private key as bitcoinj" in {
    val bitcoinjPublicKeyBytes = CryptoTestUtil.bitcoinjPrivateKey.getPubKey
    CryptoTestUtil.privateKey.publicKey.hex must be (BitcoinSUtil.encodeHex(bitcoinjPublicKeyBytes))
  }

  it must "create a bitcoin-s private key from a bitcoinj private key, then convert to the same public key" in {
    val bitcoinjKey = new org.bitcoinj.core.ECKey()
    val bitcoinsPrivKey = ECPrivateKey(bitcoinjKey.getSecretBytes)
    val bitcoinsPublicKey = bitcoinsPrivKey.publicKey
    val bitcoinjPublicKey = bitcoinjKey.getPubKey

    bitcoinsPublicKey.bytes must be (bitcoinjPublicKey)
  }

  it must "create a bitcionj private key from a bitcoins private key and get the same public key" in {
    val bitcoinsPrivKey = ECPrivateKey.freshPrivateKey
    val bitcoinjPrivKey = org.bitcoinj.core.ECKey.fromPrivate(bitcoinsPrivKey.bytes.toArray)
    val bitcoinjPublicKey = bitcoinjPrivKey.getPubKey
    val bitcoinsPublicKey = bitcoinsPrivKey.publicKey

    bitcoinsPublicKey.bytes must be (bitcoinjPublicKey)
  }

  it must "create a private key from the dumped base58 in bitcoin-cli" in {
    val privateKeyBase58 = CryptoTestUtil.privateKeyBase58
    val bitcoinjDumpedPrivateKey = new org.bitcoinj.core.DumpedPrivateKey(BitcoinJTestUtil.params,privateKeyBase58)
    val bitcoinjPrivateKey = bitcoinjDumpedPrivateKey.getKey
    val privateKey = ECPrivateKey.fromWIFToPrivateKey(privateKeyBase58)
    privateKey.hex must be (bitcoinjPrivateKey.getPrivateKeyAsHex)
  }

  it must "create a private key from a sequence of bytes that has the same byte representation of bitcoinj ECKeys" in {
    val bytes = CryptoTestUtil.bitcoinjPrivateKey.getPrivKeyBytes.toList
    val bitcoinJKey = org.bitcoinj.core.ECKey.fromPrivate(bytes.toArray)
    val privateKey : ECPrivateKey = ECPrivateKey(bytes)
    privateKey.hex must be (bitcoinJKey.getPrivateKeyAsHex)
  }

  it must "create a private key from bytes" in {
    val privKeyBytes = Seq(0.toByte)
    ECPrivateKey(privKeyBytes).bytes must be (privKeyBytes)
  }

  it must "create a private key from its hex representation" in {
    val privateKeyHex = "180cb41c7c600be951b5d3d0a7334acc7506173875834f7a6c4c786a28fcbb19"
    val key: ECPrivateKey = ECPrivateKey(privateKeyHex)
    key.hex must be (privateKeyHex)
  }

  it must "determine if a private key corresponds to a compressed public key or not" in {
    val compressedKey = "L1RrrnXkcKut5DEMwtDthjwRcTTwED36thyL1DebVrKuwvohjMNi"
    val uncompressedKey = "93DVKyFYwSN6wEo3E2fCrFPUp17FtrtNi2Lf7n4G3garFb16CRj"
    val invalidCompressedByteSeq : Seq[Byte] = Seq(-128, 85, -55, -68, -53, -98, -42, -124, 70, -47, -73, 82, 115,
      -69, -50, -119, -41, -2, 1, 58, -118, -51, 22, 37, 81, 68, 32, -5, 42, -54, 26, 33, -60, -1, -103, -95, 111, -44)
    ECPrivateKey.isCompressed(compressedKey) must be (true)
    ECPrivateKey.isCompressed(uncompressedKey) must be (false)
    ECPrivateKey.isCompressed(invalidCompressedByteSeq) must be (false)
  }

  it must "create a fresh private key" in {
    ECPrivateKey.apply().isInstanceOf[ECPrivateKey] must be (true)
  }

}
