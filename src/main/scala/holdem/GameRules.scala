package holdem

object GameRules {
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
  implicit def setStringsToSetCards(s: Set[String]): Set[Card] = s.map(_.toCard)
  implicit def setSetStringsToSetSetCards(s: Set[Set[String]]): Set[Set[Card]] = s.map(setStringsToSetCards)
 
  
  def highcard(cardOne: Card, cardTwo: Card) = {
    if (cardOne.rank > cardTwo.rank) cardOne else cardTwo
  }  
  
}