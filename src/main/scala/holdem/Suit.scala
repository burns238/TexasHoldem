package holdem

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