package org.bitcoins.core.gen

import org.bitcoins.core.number.{Int32, Int64, UInt32, UInt64}
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.script.constant.ScriptNumber
import org.bitcoins.core.util.NumberUtil
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary

/**
  * Created by chris on 6/16/16.
  */
trait NumberGenerator {

  /**
    * Creates a generator that generates positive long numbers
    *
    * @return
    */
  def positiveLongs: Gen[Long] = Gen.choose(0, Long.MaxValue)

  /**
    * Creates a generator for positive longs without the number zero
    *
    * @return
    */
  def positiveLongsNoZero : Gen[Long] = Gen.choose(1,Long.MaxValue)

  /**
    * Creates a number generator that generates negative long numbers
    *
    * @return
    */
  def negativeLongs: Gen[Long] = Gen.choose(Long.MinValue,-1)


  /**
    * Generates a number in the range 0 <= x <= 2 ^^32 - 1
    * then wraps it in a UInt32
    *
    * @return
    */
  def uInt32s: Gen[UInt32] = Gen.choose(0L,(NumberUtil.pow2(32)-1).toLong).map(UInt32(_))


  /**
    * Chooses a BigInt in the ranges of 0 <= bigInt < 2^^64
    *
    * @return
    */
  def bigInts : Gen[BigInt] = Gen.chooseNum(Long.MinValue,Long.MaxValue)
    .map(x => BigInt(x) + BigInt(2).pow(63))

  def positiveBigInts : Gen[BigInt] = bigInts.filter(_ >= 0)

  def bigIntsUInt64Range : Gen[BigInt] = positiveBigInts.filter(_ < (BigInt(1) << 64))

  /**
    * Generates a number in the range 0 <= x < 2^^64
    * then wraps it in a UInt64
    *
    * @return
    */
  def uInt64s : Gen[UInt64] = for {
    bigInt <- bigIntsUInt64Range
  } yield UInt64(bigInt)


  def int32s : Gen[Int32] = Gen.choose(Int32.min.underlying,Int32.max.underlying).map(Int32(_))

  def int64s : Gen[Int64] = Gen.choose(Int64.min.underlying, Int64.max.underlying).map(Int64(_))

  def scriptNumbers: Gen[ScriptNumber] = Gen.choose(Int64.min.underlying, Int64.max.underlying).map(ScriptNumber(_))

  def compactSizeUInts : Gen[CompactSizeUInt] = uInt64s.map(CompactSizeUInt(_))

  /**
    * Generates an arbitrary [[Byte]] in Scala
    * @return
    */
  def byte: Gen[Byte] = arbitrary[Byte]

  /**
    * Generates a 100 byte sequence
    * @return
    */
  def bytes: Gen[Seq[Byte]] = for {
    num <- Gen.choose(0,100)
    b <- bytes(num)
  } yield b

  /**
    * Generates the number of bytes specified by num
    * @param num
    * @return
    */
  def bytes(num : Int): Gen[Seq[Byte]] = Gen.listOfN(num,byte)

  /** Generates a random boolean */
  def bool: Gen[Boolean] = for {
    num <- Gen.choose(0,1)
  } yield num == 1

  /** Generates a bit vector */
  def bitVector: Gen[Seq[Boolean]] = for {
    vector <- Gen.listOfN(8,bool)
  } yield vector

  /** Generates a sequence of bit vectors */
  def bitVectors: Gen[Seq[Seq[Boolean]]] = for {
    num <- Gen.choose(0,100)
    vectors <- Gen.listOfN(num,bitVector)
  } yield vectors
}

object NumberGenerator extends NumberGenerator
