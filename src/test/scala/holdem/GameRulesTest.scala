package holdem

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import GameRules._

class GameRulesTest extends FlatSpec with Matchers {
  
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
  it should "be able to determine if a straight exists in two lists" in {
    val hand1 = Set("AC","2H","3D","5H","QH","4S","AS")
    val hand2 = Set("AC","3H","5D","7H","9H","10S","AS")
    Hand(hand1).isStraight shouldBe true
    Hand(hand2).isStraight shouldBe false
  }
  it should "be able to determine if a flush exists " in {
    val hand1 = Set("AD","2D","3D","4D","5D","QH","AS") 
    val hand2 = Set("AC","3H","5D","7H","9H","10S","AS")
    Hand(hand1).isFlush shouldBe true
    Hand(hand2).isFlush shouldBe false
  }
  it should "be able to determine if a straight flush exists" in {
    val hand = Set("AD","10D","JD","5H","QD","KD","AS")
    Hand(hand).isStraightFlush shouldBe true
  }
  it should "not find a straight flush in hand 3S, 4S, dealer 3C, 5S, KC, QC, 5H" in {
    val hand = Set("3S","4S","3C","5S","KC","QC","5H")
    Hand(hand).isStraightFlush shouldBe false
  }
  
  it should "not find a straight flush in hand where the straight is not the flush" in {
    val hand = Set("AD","2D","3D","4D","5H","QD","AS") 
    Hand(hand).isStraightFlush shouldBe false
  }
  
}