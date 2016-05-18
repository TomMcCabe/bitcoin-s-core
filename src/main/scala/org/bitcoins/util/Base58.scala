package org.bitcoins.util

import scala.annotation.tailrec

/**
  * Created by chris on 5/16/16.
  * source of values: https://en.bitcoin.it/wiki/Base58Check_encoding
  */
trait Base58 {

  val base58Characters = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  val base58CharactersArray = base58Characters.toCharArray
  val validBase58Bytes : Seq[Byte] = for {
    i<-0 to 57
  } yield i.toByte

  def decode(chars: Seq[Char]): Seq[Byte] = {
    @tailrec
    def loop(chars : Seq[Char], accum : List[Byte]) : Seq[Byte] = {
      chars match {
        case Nil => accum
        case h :: t => loop(t, decode(h) :: accum)
      }
    }
    loop(chars.toList, List()).reverse
  }

  /**
    * Encode sequence of bytes into a base58 string
    * @param bytes
    * @return
    */

  def encode(bytes: Seq[Byte]) : String = {
    @tailrec
    def loop(bytes : Seq[Byte], accum : List[Char]) : List[Char] = {
      bytes match {
        case Nil => accum.reverse
        case h :: t => loop(t, encode(h.toByte) :: accum)
      }
    }
    loop(bytes, List()).mkString
  }

  def encode(byte : Byte) : Char = base58CharactersArray(byte.toByte)

  /**
    * Takes in a base58 string and converts it into a sequence of chars
    *
    * @param base58
    * @return the sequence of chars representing the base58 string
    */
  def decode(base58: String) : Seq[Byte] = {
    decode(base58.toCharArray)
  }

  def decode(char : Char) : Byte = {
  //TODO: improve/simplify decode function (remove iteration)

    if (char == '1') 0
    else if (char == '2') 1
    else if (char == '3') 2
    else if (char == '4') 3
    else if (char == '5') 4
    else if (char == '6') 5
    else if (char == '7') 6
    else if (char == '8') 7
    else if (char == '9') 8
    else if (char == 'A') 9
    else if (char == 'B') 10
    else if (char == 'C') 11
    else if (char == 'D') 12
    else if (char == 'E') 13
    else if (char == 'F') 14
    else if (char == 'G') 15
    else if (char == 'H') 16
    else if (char == 'J') 17
    else if (char == 'K') 18
    else if (char == 'L') 19
    else if (char == 'M') 20
    else if (char == 'N') 21
    else if (char == 'P') 22
    else if (char == 'Q') 23
    else if (char == 'R') 24
    else if (char == 'S') 25
    else if (char == 'T') 26
    else if (char == 'U') 27
    else if (char == 'V') 28
    else if (char == 'W') 29
    else if (char == 'X') 30
    else if (char == 'Y') 31
    else if (char == 'Z') 32
    else if (char == 'a') 33
    else if (char == 'b') 34
    else if (char == 'c') 35
    else if (char == 'd') 36
    else if (char == 'e') 37
    else if (char == 'f') 38
    else if (char == 'g') 39
    else if (char == 'h') 40
    else if (char == 'i') 41
    else if (char == 'j') 42
    else if (char == 'k') 43
    else if (char == 'm') 44
    else if (char == 'n') 45
    else if (char == 'o') 46
    else if (char == 'p') 47
    else if (char == 'q') 48
    else if (char == 'r') 49
    else if (char == 's') 50
    else if (char == 't') 51
    else if (char == 'u') 52
    else if (char == 'v') 53
    else if (char == 'w') 54
    else if (char == 'x') 55
    else if (char == 'y') 56
    else if (char == 'z') 57
    else char.toByte
  }
}

object Base58 extends Base58



