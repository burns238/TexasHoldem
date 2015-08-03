
package holdem

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class HoldemTest extends FlatSpec with Matchers {
  
  holdem = new Holdem
  
  "The 5H card" should "be called the five of hearts" in {
    Holdem.Card('5', 'H').toString shouldBe "5 of Hearts"
  }
  
}