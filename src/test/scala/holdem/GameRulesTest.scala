package holdem

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import GameRules._

class GameRulesTest extends FlatSpec with Matchers {
  
  // Ranking table:
  //
  // Royal Flush
  // Four of a kind
  // Full house
  // Flush
  // Straight
  // Three of a kind
  // Two pair
  // Pair
  // High card
  
  "The game" should "be able to determine the high card" in {
    highcard("AD".toCard, "KH".toCard) shouldBe "AD".toCard
    highcard("QS".toCard, "5C".toCard) shouldBe "QS".toCard
    highcard("6C".toCard, "8C".toCard) shouldBe "8C".toCard
  }
  it should "be able to compare cards with strings" in {
    Card(Rank("A"), Diamonds) shouldBe "AD".toCard 
    Card(Rank("Q"), Spades) shouldBe "QS".toCard
    Card(Rank("8"), Clubs) shouldBe "8C".toCard
  }
  it should "be able to compare strings with cards" in {
    "AD".toCard shouldBe Card(Rank("A"), Diamonds)
    "QS".toCard shouldBe Card(Rank("Q"), Spades)
    "8C".toCard shouldBe Card(Rank("8"), Clubs)
  }
  it should "know which ranks are adjacent" in {
    Rank("5") adjacentTo Rank("4") shouldBe true
    Rank("4") adjacentTo Rank("5") shouldBe true
    Rank("Q") adjacentTo Rank("K") shouldBe true
    Rank("K") adjacentTo Rank("Q") shouldBe true
    Rank("10") adjacentTo Rank("J") shouldBe true
    Rank("J") adjacentTo Rank("10") shouldBe true
    Rank("A") adjacentTo Rank("2") shouldBe true
    Rank("2") adjacentTo Rank("A") shouldBe true
    Rank("A") adjacentTo Rank("K") shouldBe true
    Rank("K") adjacentTo Rank("A") shouldBe true
    Rank("K") adjacentTo Rank("J") shouldBe false
    Rank("J") adjacentTo Rank("K") shouldBe false
    Rank("K") adjacentTo Rank("4") shouldBe false
    Rank("4") adjacentTo Rank("K") shouldBe false
    Rank("10") adjacentTo Rank("8") shouldBe false
    Rank("8") adjacentTo Rank("10") shouldBe false
    Rank("A") adjacentTo Rank("3") shouldBe false
    Rank("3") adjacentTo Rank("A") shouldBe false
  }
  

}