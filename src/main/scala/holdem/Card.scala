package holdem

case class Card(rank: Rank, suit: Suit) extends Ordered[Card] { 
  def name = rank.rankName + " of " + suit.suitName
    
  override def toString: String = {
    List(rank.r.toString, suit.toString).mkString
  }
  
  def compare(that: Card): Int = this.rank.rankScore compare that.rank.rankScore

}