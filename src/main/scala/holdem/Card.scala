package holdem

case class Card(rank: Rank, suit: Suit) { 
  def name = rank.rankName + " of " + suit.suitName
    
  override def toString: String = {
    List(rank.r.toString, suit.toString).mkString
  }

}