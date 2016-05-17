package org.bitcoins.util

import CryptoUtil._

/**
  * Created by chris on 5/16/16.
  */
trait Base58 {

  /**
    * Takes the given sequence of chars and converts it into a base58 string
    *
    * @param chars
    * @return the base58 string
    */
  def encode(chars: Seq[Char]): String = {


    val base58Characters = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
    /*
    val version : char
    val payLoad
     */
    ???
  }

  def encode(byte: Byte) : Char = {
    if (byte == 0) '1'
    else if (byte == 1) '2'
    else if (byte == 2) '3'
    else if (byte == 3) '4'
    else if (byte == 4) '5'
    else if (byte == 5) '6'
    else if (byte == 6) '7'
    else if (byte == 7) '8'
    else if (byte == 8) '9'
    else if (byte == 9) 'A'
    else if (byte == 10) 'B'
    else if (byte == 10) 'C'
    else if (byte == 12) 'D'
    else if (byte == 13) 'E'
    else if (byte == 14) 'F'
    else if (byte == 15) 'G'
    else if (byte == 16) 'H'
    else if (byte == 17) 'J'
    else if (byte == 18) 'K'
    else if (byte == 19) 'L'
    else if (byte == 20) 'M'
    else if (byte == 21) 'N'
    else if (byte == 22) 'P'
    else if (byte == 23) 'Q'
    else if (byte == 24) 'R'
    else if (byte == 25) 'S'
    else if (byte == 26) 'T'
    else if (byte == 27) 'U'
    else if (byte == 28) 'V'
    else if (byte == 29) 'W'
    else if (byte == 30) 'X'
    else if (byte == 31) 'Y'
    else if (byte == 32) 'Z'
    else if (byte == 33) 'a'
    else if (byte == 34) 'b'
    else if (byte == 35) 'c'
    else if (byte == 36) 'd'
    else if (byte == 37) 'e'
    else if (byte == 38) 'f'
    else if (byte == 39) 'g'
    else if (byte == 40) 'h'
    else if (byte == 41) 'i'
    else if (byte == 42) 'j'
    else if (byte == 43) 'k'
    else if (byte == 44) 'm'
    else if (byte == 45) 'n'
    else if (byte == 46) 'o'
    else if (byte == 47) 'p'
    else if (byte == 48) 'q'
    else if (byte == 49) 'r'
    else if (byte == 50) 's'
    else if (byte == 51) 't'
    else if (byte == 52) 'u'
    else if (byte == 53) 'v'
    else if (byte == 54) 'w'
    else if (byte == 55) 'x'
    else if (byte == 56) 'y'
    else if (byte == 57) 'z'
    else byte.toChar

  }

  /**
    * Takes in a base58 string and converts it into a sequence of chars
    *
    * @param base58
    * @return the sequence of chars representing the base58 string
    */
  def decode(base58: String): Seq[Char] = ???

  def decode(char : Char) : Byte = {

   /*
   val numbers = for {
      i<-0 to 8
    } yield Map(i->(i+1))


    println("chars less than 9: " + numbers)
    */

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
