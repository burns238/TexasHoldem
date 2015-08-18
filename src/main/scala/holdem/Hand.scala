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
  
  def rankGroups: Set[Set[Card]] = {
    val groups = cards.groupBy(_.rank).map(x=>x._2).toSet
    groups.filter(_.size > 1)
  }
  
  def rankGroupsAndTotalSize(groups: Int, totalSize: Int): Boolean = {
    rankGroups.size == groups && rankGroups.map(_.size).sum == totalSize
  }
  
  def isPair = rankGroupsAndTotalSize(1,2)
  def isThreeOfAKind = rankGroupsAndTotalSize(1,3)
  def isFourOfAKind = rankGroupsAndTotalSize(1,4)
  def isTwoPairs = rankGroupsAndTotalSize(2,4)
  def isFullHouse = rankGroupsAndTotalSize(2,5)
  
  def evaluate = cards match {
    case isPair => PairHand(rankGroups.head,completeHand((cards.--(rankGroups.head)),3))  
  }
  
  def completeHand(cards: Set[Card], number: Int) = {
    val byRank = cards.toList.sorted.reverse
    (byRank.take(number)).toSet
  }
}

case class PairHand(pair: Set[Card], rest: Set[Card]) 
