package holdem

case class Hand(cards: Set[Card]) {
  
  def isStraight: Boolean = {
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
    val cardRanks = cards.map(_.rank)
    allRunsOfLength(5, cardRanks).size > 0
  }
  
  def isFlush: Boolean = {
    val suits = Set(Diamonds, Hearts, Clubs, Spades)
    //val cardSuits = cards.map(_.suit)
    val suitCounts = for {
      suit <- suits
    } yield cards count (_.suit == suit)  
    suitCounts.max >= 5
  }
  
  def isStraightFlush: Boolean = {
    if (isFlush && isStraight) true
    else false  
  }
  
}
