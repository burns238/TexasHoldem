
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
    
    def compare(that: Rank) = {
      this.rankScore - that.rankScore
    }
  }
  
  def Card (rank: Char, suit: Char) = {    
    getRank(rank) + " of " + getSuit(suit)
  }
  
  def getRank(rank: Char) = rank match {
      case 'A' => "Ace"
      case 'K' => "King"
      case 'Q' => "Queen"
      case 'J' => "Jack"
      case _   => rank.toString  
  }
  
  def getSuit(suit: Char) = suit match {
      case 'H' => "Hearts"
      case 'D' => "Diamonds"
      case 'S' => "Spades"
      case 'C' => "Clubs"  
      case _   => suit.toString  
  }
 
  def Highcard(cardOne: String, cardTwo: String) = {
    val rankOne = Rank(cardOne.head)
    val rankTwo = Rank(cardTwo.head)
    if (rankOne > rankTwo) cardOne else cardTwo 
  }
}

class HoldemTest extends FlatSpec with Matchers {
  
  "The 5H card" should "be called the five of hearts" in {
    Holdem.Card('5', 'H').toString shouldBe "5 of Hearts"
  }
  
  "The game" should "be able to determine the high card" in {
    Holdem.Highcard("AD", "KH") shouldBe "AD"  
    Holdem.Highcard("QS", "5C") shouldBe "QS"
    Holdem.Highcard("6C", "8C") shouldBe "8C" 
  }
  
  "Cards" should "have a rank" in {
    Holdem.Card('5', 'H').rank shouldBe Holdem.Rank('5')
    Holdem.Card('Q', 'S').rank shouldBe Holdem.Rank('Q')
  }
  
}