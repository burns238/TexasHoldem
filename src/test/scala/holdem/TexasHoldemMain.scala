
package holdem

import org.scalatest.FlatSpec
import org.scalatest.Matchers

object Holdem {
  
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
  
  
}