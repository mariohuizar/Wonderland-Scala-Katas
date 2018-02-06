import AlphabetCipher._
import org.scalatest._

class AlphabetCipherSpec extends FlatSpec with Matchers {
  "encode" should "encode given a secret keyword" in {
    assert(encode("bcd", "meet") == "nghu")
    assert(
      encode("vigilance", "meetmeontuesdayeveningatseven") == "hmkbxebpxpmyllyrxiiqtoltfgzzv")
    assert(encode("scones", "meetmebythetree") == "egsgqwtahuiljgs")
  }

  "decode" should "decode an encyrpted message given a secret keyword" in {
    assert(decode("bcd", "nghu") == "meet")
    assert(
      decode("vigilance", "hmkbxebpxpmyllyrxiiqtoltfgzzv") == "meetmeontuesdayeveningatseven")
    assert(decode("scones", "egsgqwtahuiljgs") == "meetmebythetree")

  }

  "decipher" should "extract the secret keyword given an encrypted message and the original message" in {
    assert(decipher("hmkbxebpxpmyllyrxiiqtoltfgzzv",
                    "meetmeontuesdayeveningatseven") == "vigilance")
    assert(
      decipher("opkyfipmfmwcvqoklyhxywgeecpvhelzg",
               "thequickbrownfoxjumpsoveralazydog") == "vigilance")
    assert(
      decipher("hcqxqqtqljmlzhwiivgbsapaiwcenmyu",
               "packmyboxwithfivedozenliquorjugs") == "scones")
  }

  "rotate" should "rotate things" in {
    assert(rotate("abc".toVector, 1) == "bca".toVector)
    assert(rotate("abc".toVector, 3) == "abc".toVector)
  }

  "findRepeatingWord" should "find repeating words" in {
    assert((findRepeatingWord("abcabc", "a", 1) == "abc"))
    assert((findRepeatingWord("abc", "a", 1) == "abc"))
    assert((findRepeatingWord("abcabcab", "a", 1) == "abc"))
  }

}
