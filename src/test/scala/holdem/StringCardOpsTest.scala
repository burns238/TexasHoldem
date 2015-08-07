package holdem

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import GameRules._

class StringCardOpsTest extends FlatSpec with Matchers {
  
  "toCard" should "accept the Queen of Hearts" in {
    "QH".toCard
  }
  it should "accept the Ace of Spades" in {
    "AS".toCard
  }
  it should "accept the Five of clubs" in {
    "5C".toCard
  }
  it should "not accept 0S" in {
    intercept[IllegalArgumentException] {
      "0S".toCard
    }
  }
  it should "not accept 3W" in {
    intercept[IllegalArgumentException] {
      "3W".toCard
    }
  }
  it should "not accept 10Q" in {
    intercept[IllegalArgumentException] {
      "10Q".toCard
    }
  }
  it should "not accept QQ" in {
    intercept[IllegalArgumentException] {
      "QQ".toCard
    }
  }
  it should "not accept 1S" in {
    intercept[IllegalArgumentException] {
      "1S".toCard
    }
  }
  it should "not accept 1" in {
    intercept[IllegalArgumentException] {
      "1".toCard
    }
  }
  it should "not accept S" in {
    intercept[IllegalArgumentException] {
      "S".toCard
    }
  }
  it should "not accept 123ABC" in {
    intercept[IllegalArgumentException] {
      "123ABC".toCard
    }
  }
  it should "not accept the empty string" in {
    intercept[MatchError] {
      "".toCard
    }
  }
}