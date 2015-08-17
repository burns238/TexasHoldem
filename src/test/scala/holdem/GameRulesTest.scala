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
  it should "find two pairs in 4H, 4C, 5H, 9S, 5S" in {
    val hand = Set("4H", "4C", "5H", "9S", "5S")
    Hand(hand).rankGroups shouldBe Set(Set("4H".toCard, "4C".toCard), Set("5H".toCard, "5S".toCard))
  }
  it should "find three of a kind in KC, KS, 3D, 4H, KD" in {
    val hand = Set("KC", "KS", "3D", "4H", "KD")
    Hand(hand).rankGroups shouldBe Set(Set("KC".toCard,"KS".toCard,"KD".toCard))
  }
  it should "find a pair in AC, 4S, 5D, JH, 4D" in {
    val hand = Set("AC", "4S", "5D", "JH", "4D")
    Hand(hand).rankGroups shouldBe Set(Set("4S".toCard, "4D".toCard))
  }
  it should "find no groups in AC, 9D, 8S, 7S, QH" in {
    val hand = Set("AC", "9D", "8S", "7S", "QH")
    Hand(hand).rankGroups shouldBe Set[Card]()
  }
  it should "find two pairs in this set of rankGroups" in {
    val hand = Set("4H", "4C", "5H", "9S", "5S")
    Hand(hand_).isTwoPairs shouldBe true
    Hand(hand_).isPair shouldBe false
  }
  it should "find three of a kind in this set of rankGroups" in {
    val hand = Set("KC", "KS", "3D", "4H", "KD")
    Hand(hand_).isThreeOfAKind shouldBe true
    Hand(hand_).isPair shouldBe false
  }  
  it should "find a pair in this set of rankGroups" in {
    val hand = Set("AC", "4S", "5D", "JH", "4D")
    Hand(hand_).isPair shouldBe true
  }  
  it should "find four of a kind in this set of rankGroups" in {
    val hand = Set("AC", "AS", "AD", "AH", "4D")
    Hand(hand_).isFourOfAKind shouldBe true
    Hand(hand_).isThreeOfAKind shouldBe false
    Hand(hand_).isPair shouldBe false
  }  
  it should "find a full house in this set of rankGroups" in {
    val hand = Set("AC", "AS", "AD", "4H", "4D")
    Hand(hand_).isFullHouse shouldBe true
    Hand(hand_).isThreeOfAKind shouldBe false
    Hand(hand_).isPair shouldBe false
  }  
}