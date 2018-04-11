package holdem

case class Rank(r: String) extends Ordered[Rank]{
  require(List("A","2","3","4","5","6","7","8","9","10","J","Q","K") contains r) 
  
  def rankScore: Int = r match {
    case "A" => 14
    case "K" => 13
    case "Q" => 12
    case "J" => 11
    case _   => r.toInt       
  }
  
  def rankName = r match {
    case "A" => "Ace"
    case "K" => "King"
    case "Q" => "Queen"
    case "J" => "Jack"
    case _   => r  
  }
  
  def compare(that: Rank) = {
    this.rankScore - that.rankScore
  }
  
  def equals(that: Rank) = {
    this.r == that.r
  }
  
  def adjacentTo(that: Rank) = {
    ((this.rankScore - that.rankScore).abs == 1) ||
    (this.r == "A" && that.r == "2") ||
    (this.r == "2" && that.r == "A")
  }
}