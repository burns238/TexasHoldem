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
 
  def highcard(cardOne: Card, cardTwo: Card) = {
    if (cardOne.rank > cardTwo.rank) cardOne else cardTwo 
  }
  
  def isStraight(hand: List[Card], dealer: List[Card]): Boolean = {
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
    val cardRanks = (hand.toSet union dealer.toSet).map(_.rank)
    allRunsOfLength(5, cardRanks).size > 0
  }
  def isFlush(hand: List[Card], dealer: List[Card]): Boolean = {
    val suits = Set(Diamonds, Hearts, Clubs, Spades)
    val cardSuits = (hand ::: dealer).map(_.suit)
    val suitCounts = for {
      suit <- suits
    } yield cardSuits count (_ == suit)
    suitCounts.max >= 5
  }
  def isRoyalFlush(hand: List[Card], dealer: List[Card]): Boolean = true
}