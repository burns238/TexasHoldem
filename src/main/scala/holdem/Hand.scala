package holdem

import scala.collection.immutable
import scala.math.Ordering.Implicits._

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
    potentialFlushCards.size >= 5
  }
  
  def isStraightFlush: Boolean = {
    Hand(potentialFlushCards).isStraight
  }
  
  val allRankGroupsInOrder: List[Set[Card]] = {
    cards.groupBy(_.rank)
      .map(x=>x._2)
      .toList
      .sortWith((a, b) => (a.size > b.size) || (a.size == b.size) && a.head > b.head)
  }
  val rankGroupProfile = allRankGroupsInOrder.map(_.size)
  
  def rankGroupsHaveProfile(profile: List[Int]) = {
    (rankGroupProfile take 2) == profile
  }
  
  val isPair = rankGroupsHaveProfile(List(2,1))
  val isThreeOfAKind = rankGroupsHaveProfile(List(3,1))
  val isFourOfAKind = rankGroupsHaveProfile(List(4,1))
  val isTwoPairs = rankGroupsHaveProfile(List(2,2))
  val isFullHouse = rankGroupsHaveProfile(List(3,2))
  
  def evaluate = {
    lazy val set1 = allRankGroupsInOrder.head
    lazy val set2 = allRankGroupsInOrder.tail.head
    lazy val allCards = allRankGroupsInOrder.flatMap(_.toList)
    true match {
      case `isFullHouse` => FullHouseHand(set1, set2)
      case `isFourOfAKind` => FourOfAKindHand(set1, allCards drop 4 take 1 toSet)
      case `isThreeOfAKind` => ThreeOfAKindHand(set1, allCards drop 3 take 2 toSet)
      case `isTwoPairs` => TwoPairHand(set1, set2, allCards drop 4 take 1 toSet)
      case `isPair` => PairHand(set1, allCards drop 2 take 3 toSet)
      case _ => HighCardHand(allCards take 5 toSet)
    }
  }
  
  def completeHand(cards: Set[Card], number: Int) = {
    val byRank = cards.toList.sorted.reverse
    (byRank.take(number)).toSet
  }
}

case class HighCardHand(rest: Set[Card])
case class PairHand(pair: Set[Card], rest: Set[Card])
case class ThreeOfAKindHand(three: Set[Card], rest: Set[Card])
case class FourOfAKindHand(four: Set[Card], rest: Set[Card])
case class TwoPairHand(higherPair: Set[Card], lowerPair: Set[Card], rest: Set[Card])
case class FullHouseHand(three: Set[Card], pair: Set[Card])
