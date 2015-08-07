
package holdem

import org.scalatest.FlatSpec
import org.scalatest.Matchers

object Holdem {
  
  implicit class StringCardOps(s: String) {
   
    def toCard: Card = s.toList match {
      case '1' :: '0' :: s => Card(Rank("10"), s.mkString.toSuit)
      case r :: s => Card(Rank(r.toString), s.mkString.toSuit)
      case _ => throw new MatchError(s + " is not a valid card")
    }    
    
    def toSuit: Suit = {
      require(List("D","H","C","S") contains s)
      s match {
      case "D" => Diamonds
      case "H" => Hearts
      case "C" => Clubs
      case "S" => Spades
      } 
    } 
  }
  
  implicit def listStringsToListCards(s: List[String]): List[Card] = s.map(_.toCard)
  
  case class Rank(r: String) extends Ordered[Rank]{
    require(List("A","2","3","4","5","6","7","8","9","10","J","Q","K") contains r) 
    
    def rankScore = r match {
      case "A" => 14
      case "K" => 13
      case "Q" => 12
      case "J" => 11
      case _   => r.toInt       
    }
    
    def rankName = r match {
      case "A" => "Ace"
      case "K" => "King"
      case "Q" => "Queen"
      case "J" => "Jack"
      case _   => r  
    }
    
    def compare(that: Rank) = {
      this.rankScore - that.rankScore
    }
    
    def equals(that: Rank) = {
      this.r == that.r
    }
    
    def adjacentTo(that: Rank) = {
      ((this.rankScore - that.rankScore).abs == 1) ||
      (this.r == "A" && that.r == "2") ||
      (this.r == "2" && that.r == "A")
    }
  }
 
 class Suit(s:String) {

    def suitName = s match {
      case "H" => "Hearts"
      case "D" => "Diamonds"
      case "S" => "Spades"
      case "C" => "Clubs"  
      case _   => throw new MatchError (s + " is not a valid rank.")    
    }
    
  }
    
  object Diamonds extends Suit("D")
  object Hearts extends Suit("H")
  object Clubs extends Suit("C")
  object Spades extends Suit("S")
  
  
  case class Card(rank: Rank, suit: Suit) { 
    def name = rank.rankName + " of " + suit.suitName
    
    
    
    override def toString: String = {
      List(rank.r.toString, suit.toString).mkString
    }

  }
 
  def Highcard(cardOne: Card, cardTwo: Card) = {
    if (cardOne.rank > cardTwo.rank) cardOne else cardTwo 
  }
  
  def IsStraight(hand: List[Card], dealer: List[Card]): Boolean = {
    def allRunsOfLength(length: Int, ranks: Set[Rank]): Set[Set[Rank]] = {
      if (length == 1) Set(ranks)
      else {
        for {
          rank <- ranks
          run <- allRunsOfLength(length - 1, ranks - rank)
          if run.exists(x => x adjacentTo rank)
        } yield run + rank
      }
    }
    // cardRanks is a set, so it doesn't contain duplicates. 
    // This means any repeated ranks only appear once.
    val cardRanks = (hand.toSet union dealer.toSet).map(_.rank)
    allRunsOfLength(5, cardRanks).size > 0
  }
  
  def IsFlush(hand: List[Card], dealer: List[Card]): Boolean = {
    val suits = Set(Diamonds, Hearts, Clubs, Spades)
    val cardSuits = (hand ::: dealer).map(_.suit)
    val suitCounts = for {
      suit <- suits
    } yield cardSuits count (_ == suit)
    suitCounts.max >= 5
  }
}

class HoldemTest extends FlatSpec with Matchers {
  
  import Holdem._
  
  "The 5H card" should "be called the five of hearts" in {
    "5H".toCard.name shouldBe "5 of Hearts"
  }
  
  "The game" should "be able to determine the high card" in {
    Highcard("AD".toCard, "KH".toCard) shouldBe "AD".toCard
    Highcard("QS".toCard, "5C".toCard) shouldBe "QS".toCard
    Highcard("6C".toCard, "8C".toCard) shouldBe "8C".toCard
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
    val hand = List("AC","2H")
    val dealer = List("3D","5H","QH","4S","AS")
    IsStraight(hand,dealer) shouldBe true
    val hand2 = List("AC","3H")
    val dealer2 = List("5D","7H","9H","10S","AS")
    IsStraight(hand2,dealer2) shouldBe false
  }
  
  it should "be able to determine if a flush exists " in {
    val hand = List("AD","2D")
    val dealer = List("3D","5H","QD","4D","AS")
    IsFlush(hand,dealer) shouldBe true
    val hand2 = List("AC","3H")
    val dealer2 = List("5D","7H","9H","10S","AS")
    IsFlush(hand2,dealer2) shouldBe false
  }
    
  "Cards" should "have a rank" in {
    "5H".toCard.rank shouldBe Rank("5")
    "QS".toCard.rank shouldBe Rank("Q")
  }
  
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
  it should "be able to determine if a royal flush exists" in {
    val hand = List("AD","10D")
    val dealer = List("JD","5H","QD","KD","AS")
    IsRoyalFlush(hand,dealer) shouldBe true
  }
}