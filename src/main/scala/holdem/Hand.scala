package holdem

case class Hand(cards: Set[Card]) {
  
  val suits = Set(Diamonds, Hearts, Clubs, Spades)
  
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
  
  def potentialFlushCards = {
    val suitCounts = suits.map(s => (s, cards.count(_.suit == s)))
    val mostCommonSuit = suitCounts.toList.sortWith {(s1, s2) => s1._2 > s2._2}.head._1
    cards.filter(_.suit == mostCommonSuit)
  }
  
  def isFlush: Boolean = {
    //val cardSuits = cards.map(_.suit)
//    val suitCounts = for {
//      suit <- suits
//    } yield cards count (_.suit == suit)  
//    suitCounts.max >= 5
    potentialFlushCards.size >= 5
  }
  
  def isStraightFlush: Boolean = {
    Hand(potentialFlushCards).isStraight
  }
  
}
