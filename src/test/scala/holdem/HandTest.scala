package holdem

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import GameRules._

class HandTest extends FlatSpec with Matchers {

    "A hand" should "be able to determine if a straight exists" in {
    val hand1 = Set("AC","2H","3D","5H","QH","4S","AS")
    val hand2 = Set("AC","3H","5D","7H","9H","10S","AS")
    Hand(hand1).isStraight shouldBe true
    Hand(hand2).isStraight shouldBe false
  }
  it should "be able to determine if a flush exists" in {
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
    Hand(hand).rankGroupProfile shouldBe List(2,2,1)
  }
  it should "find three of a kind in KC, KS, 3D, 4H, KD" in {
    val hand = Set("KC", "KS", "3D", "4H", "KD")
    Hand(hand).rankGroupProfile shouldBe List(3,1,1)
  }
  it should "find a pair in AC, 4S, 5D, JH, 4D" in {
    val hand = Set("AC", "4S", "5D", "JH", "4D")
    Hand(hand).rankGroupProfile shouldBe List(2,1,1,1)
  }
  it should "find no groups in AC, 9D, 8S, 7S, QH" in {
    val hand = Set("AC", "9D", "8S", "7S", "QH")
    Hand(hand).rankGroupProfile shouldBe List(1,1,1,1,1)
  }
  it should "find two pairs in this set of rankGroups" in {
    val hand = Set("4H", "4C", "5H", "9S", "5S")
    Hand(hand).isFourOfAKind shouldBe false
    Hand(hand).isFullHouse shouldBe false
    Hand(hand).isThreeOfAKind shouldBe false
    Hand(hand).isTwoPairs shouldBe true
  }
  it should "find three of a kind in this set of rankGroups" in {
    val hand = Set("KC", "KS", "3D", "4H", "KD")
    Hand(hand).isFourOfAKind shouldBe false
    Hand(hand).isFullHouse shouldBe false
    Hand(hand).isThreeOfAKind shouldBe true
    Hand(hand).isTwoPairs shouldBe false
  }  
  it should "find a pair in this set of rankGroups" in {
    val hand = Set("AC", "4S", "5D", "JH", "4D")
    Hand(hand).isFourOfAKind shouldBe false
    Hand(hand).isFullHouse shouldBe false
    Hand(hand).isThreeOfAKind shouldBe false
    Hand(hand).isTwoPairs shouldBe false
    Hand(hand).isPair shouldBe true
  }  
  it should "find four of a kind in this set of rankGroups" in {
    val hand = Set("AC", "AS", "AD", "AH", "4D")
    Hand(hand).isFourOfAKind shouldBe true
    Hand(hand).isFullHouse shouldBe false
  }  
  it should "find a full house in this set of rankGroups" in {
    val hand = Set("AC", "AS", "AD", "4H", "4D")
    Hand(hand).isFourOfAKind shouldBe false
    Hand(hand).isFullHouse shouldBe true
  }
  
  it should "be able to evaluate a pair hand and store it as the right type, picking up the relevant high cards" in {
    val hand = Set("5C", "5D", "3S", "JH", "10C", "6D", "QC")
    Hand(hand).evaluate shouldBe PairHand(Set("5C".toCard, "5D".toCard), Set("QC".toCard, "JH".toCard, "10C".toCard))   
  }
  it should "be able to evaluate a Three of a Kind hand and store it as the right type, picking up the relevant high cards" in {
    val hand = Set("5C", "5D", "5S", "JH", "10C", "6D", "QC")
    Hand(hand).evaluate shouldBe ThreeOfAKindHand(Set("5C".toCard, "5D".toCard, "5S".toCard), Set("QC".toCard, "JH".toCard))  
  }
  it should "be able to evaluate a Four of a Kind hand and store it as the right type, picking up the relevant high cards" in {
    val hand = Set("5C", "5D", "5S", "JH", "5H", "6D", "QC")
    Hand(hand).evaluate shouldBe FourOfAKindHand(Set("5H".toCard, "5D".toCard, "5S".toCard, "5C".toCard), Set("QC".toCard))    
  }
  it should "be able to evaluate a Two Pair hand and store it as the right type, picking up the relevant high cards" in {
    val hand = Set("5C", "5D", "6S", "JH", "4C", "6D", "QC")
    Hand(hand).evaluate shouldBe TwoPairHand(Set("6S".toCard, "6D".toCard), Set("5C".toCard, "5D".toCard), Set("QC".toCard))    
  }  
  it should "be able to evaluate a Full House hand and store it as the right type, picking up the relevant high cards" in {
    val hand = Set("5C", "5D", "6S", "JH", "5H", "6D", "QC")
    Hand(hand).evaluate shouldBe FullHouseHand(Set("5C".toCard, "5D".toCard, "5H".toCard), Set("6S".toCard, "6D".toCard))    
  } 
  
  it should "know that a pair of 7s beats a pair of 4s" in {
    val pairOfSevens = Set("7D", "7H", "3S", "QH", "10D")
    val pairOfFours = Set("4C", "4D", "QS", "JH", "2S")
    (Hand(pairOfSevens) > Hand(pairOfFours)) shouldBe true
    (Hand(pairOfSevens) < Hand(pairOfFours)) shouldBe false
  }
  it should "know that in a showdown between two pairs of 10s, the one with the ace beats the one without" in {
    val pairOfTensAceHigh = Set("10C", "AH", "JC", "4S", "10S")
    val pairOfTensJackHigh = Set("JC", "10D", "10H", "3S", "9C")
    (Hand(pairOfTensAceHigh) > Hand(pairOfTensJackHigh)) shouldBe true
    (Hand(pairOfTensAceHigh) < Hand(pairOfTensJackHigh)) shouldBe false
  }

  it should "know that in a showdown between a pair of 10s and a high card, the pair wins" in {
    val pairOfTensAceHigh = Set("10C", "5H", "JC", "4S", "10S")
    val pairOfTensJackHigh = Set("JC", "10D", "AH", "3S", "9C")
    (Hand(pairOfTensAceHigh) > Hand(pairOfTensJackHigh)) shouldBe true
    (Hand(pairOfTensAceHigh) < Hand(pairOfTensJackHigh)) shouldBe false
  }
  
  
}