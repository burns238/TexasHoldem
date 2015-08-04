
package holdem

import org.scalatest.FlatSpec
import org.scalatest.Matchers

object Holdem {
  
  // The rank of a card is a specific property that we can reason about,
  // so it makes sense to make it a class.
  // Because there is an ordering of ranks, we can extend the Ordered trait,
  // which will let us implement an ordering through the 'compare' function.
  case class Rank(r: Char) extends Ordered[Rank]{
    
    def rankScore = r match {
      case 'A' => 14
      case 'K' => 13
      case 'Q' => 12
      case 'J' => 11
      case _  => r.toString.toInt
    }
    
    def rankName = r match {
      case 'A' => "Ace"
      case 'K' => "King"
      case 'Q' => "Queen"
      case 'J' => "Jack"
      case _   => r.toString  
    }
    
    def compare(that: Rank) = {
      this.rankScore - that.rankScore
    }
  }
  
  case class Card(rank: Rank, suit: Char) { 
    override def toString = rank.rankName + " of " + getSuit(suit)
  }
  

  
  def getSuit(suit: Char) = suit match {
      case 'H' => "Hearts"
      case 'D' => "Diamonds"
      case 'S' => "Spades"
      case 'C' => "Clubs"  
      case _   => suit.toString  
  }
 
  def Highcard(cardOne: Card, cardTwo: Card) = {
    if (cardOne.rank > cardTwo.rank) cardOne else cardTwo 
  }
}

class HoldemTest extends FlatSpec with Matchers {
  
  import Holdem._
  
  "The 5H card" should "be called the five of hearts" in {
    Card(Rank('5'), 'H').toString shouldBe "5 of Hearts"
  }
  
  "The game" should "be able to determine the high card" in {
    Highcard(Card(Rank('A'), 'D'), Card(Rank('K'), 'H')) shouldBe "AD"  
    Highcard(Card(Rank('Q'), 'S'), Card(Rank('5'), 'C')) shouldBe "QS"
    Highcard(Card(Rank('6'), 'C'), Card(Rank('8'), 'C')) shouldBe "8C" 
  }
  
  it should "be able to determine if a straight exists in two lists"
    val hand = List("1C","2H")
    val dealer = List("3D","5H","QH","4S","AS")
    IsStraight(hand,dealer) shouldBe true
    val hand2 = List("1C","3H")
    val dealer2 = List("5D","7H","9H","10S","AS")
    IsStraight(hand2,dealer2) shouldBe false
  
  "Cards" should "have a rank" in {
    Card(Rank('5'), 'H').rank shouldBe Rank('5')
    Card(Rank('Q'), 'S').rank shouldBe Rank('Q')
  }
  
  
  
}