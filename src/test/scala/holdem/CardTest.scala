package holdem

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class CardTest extends FlatSpec with Matchers {
  "The 5H card" should "be called the five of hearts" in {
    Card(Rank("5"), Hearts).name shouldBe "5 of Hearts"
  }
  "Cards" should "have a rank" in {
    Card(Rank("5"), Hearts).rank shouldBe Rank("5")
    Card(Rank("Q"), Spades).rank shouldBe Rank("Q")
  }
}