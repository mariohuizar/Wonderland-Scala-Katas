import scala.annotation.tailrec

object AlphabetCipher {
  val Alphabet: Vector[Char] = "abcdefghijklmnopqrstuvwxyz".toVector

  @tailrec private def rotateRecursive[T](acc: Vector[T], i: Int): Vector[T] = {
    if (i == 0) acc
    else rotateRecursive(acc.tail :+ acc.head, i - 1)
  }

  def rotate[T](list: Vector[T], n: Int = 1): Vector[T] = {
    rotateRecursive(list, n)
  }

  @tailrec def recursiveEncode(keyword: String,
                               message: String,
                               messagePos: Int,
                               acc: String): String = {
    if (messagePos == message.length) acc
    else {
      val rotations = Alphabet.indexOf(message.charAt(messagePos))
      val rotatedAlphabet = rotate(Alphabet, rotations)
      val resultChar = rotatedAlphabet(
        Alphabet.indexOf(keyword.charAt(messagePos)))
      recursiveEncode(keyword, message, messagePos + 1, acc + resultChar)
    }
  }

  @tailrec def recursiveDecodeRotations(keywordChar: Char,
                                        messageChar: Char,
                                        iter: Int): Int = {
    val rotatedAlphabet = rotate(Alphabet, iter)
    val index = Alphabet.indexOf(keywordChar)
    val char = rotatedAlphabet.charAt(index)
    if (messageChar == char) iter
    else recursiveDecodeRotations(keywordChar, messageChar, iter + 1)
  }

  def decodeRotations(keywordChar: Char, messageChar: Char) =
    recursiveDecodeRotations(keywordChar, messageChar, 0)

  @tailrec def recursiveDecode(keyword: String,
                               message: String,
                               messagePos: Int,
                               acc: String): String = {
    if (messagePos == message.length) acc
    else {
      val rotations =
        decodeRotations(keyword.charAt(messagePos), message.charAt(messagePos))
      val resultChar = Alphabet.charAt(rotations)
      recursiveDecode(keyword, message, messagePos + 1, acc + resultChar)
    }
  }

  @tailrec def recursiveDecipher(cipher: String,
                                 message: String,
                                 i: Int,
                                 acc: String): String = {
    if (i == message.length) acc
    else {
      val rotations =
        decodeRotations(cipher.charAt(i), message.charAt(i))
      val resultChar = Alphabet.charAt(rotations)
      recursiveDecipher(cipher, message, i + 1, acc + resultChar)
    }
  }

  @tailrec def enlargeKeyword(keyword: String,
                              message: String,
                              acc: String): String = {
    if (acc.length > message.length) acc.dropRight(acc.length - message.length)
    else enlargeKeyword(keyword, message, acc + keyword)
  }

  @tailrec def findRepeatingWord(keywordManyTimes: String,
                                 acc: String,
                                 pos: Int): String = {
    if (pos == keywordManyTimes.length)
      acc
    else if (acc.length + pos - 1 >= keywordManyTimes.length)
      keywordManyTimes
    else if (acc == keywordManyTimes.substring(acc.length, acc.length + pos))
      acc
    else
      findRepeatingWord(keywordManyTimes,
                        acc + keywordManyTimes.charAt(pos).toString,
                        pos + 1)
  }

  def encode(keyword: String, message: String): String = {
    val enlargedKeyword = enlargeKeyword(keyword, message, keyword)
    recursiveEncode(enlargedKeyword, message, 0, "")
  }

  def decode(keyword: String, message: String): String = {
    val enlargedKeyword = enlargeKeyword(keyword, message, keyword)
    recursiveDecode(enlargedKeyword, message, 0, "")
  }

  def decipher(cipher: String, message: String): String = {
    val keywordManyTimes = recursiveDecipher(message, cipher, 0, "")
    findRepeatingWord(keywordManyTimes, keywordManyTimes.charAt(0).toString, 1)
  }

}
