package day07

import utils.FileReader
import scala.collection.mutable.ListBuffer
import scala.util.boundary, boundary.break


class Day07_02 {
  case class Hand(handType: HandType, cards: List[Char], bid: Int)
  // ordinal = strength
  enum HandType:
    case HIGH_CARD
    case ONE_PAIR
    case TWO_PAIR
    case THREE_OF_A_KIND
    case FULL_HOUSE
    case FOUR_OF_A_KIND
    case FIVE_OF_A_KIND

  // index = strength
  val cards = List(
    'J', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'Q', 'K', 'A'
  )

  def solve() = {
    val lines =
      new FileReader().readLinesAsList("src/main/scala/day07/input.txt")
    val hands = lines.map(l => {
      val input = l.split("\\s+");
      Hand(getBestPossibleHandType(input(0)), input(0).toCharArray.toList, input(1).toInt)
    })

    val sortedHands = hands.sortWith(isWeakerHand)
    val result = sortedHands.zipWithIndex
    .map { case (hand, index) => hand.bid * (index + 1)}
    .sum
    result.toString
  }

  def isWeakerHand(hand1: Hand, hand2: Hand): Boolean = {
    if (hand1.handType != hand2.handType) then
      return hand1.handType.ordinal < hand2.handType.ordinal

    boundary {
      (0 until hand1.cards.length).foreach(i => {
        if (hand1.cards(i) != hand2.cards(i))

          break(cards.indexOf(hand1.cards(i)) < cards.indexOf(hand2.cards(i)))
      })

      false
    }
  }

  def getHandType(hand: String): HandType = {
    hand match
      case _ if isFiveOfAKind(hand)  => HandType.FIVE_OF_A_KIND
      case _ if isFourOfAKind(hand)  => HandType.FOUR_OF_A_KIND
      case _ if isFullHouse(hand)    => HandType.FULL_HOUSE
      case _ if isThreeOfAKind(hand) => HandType.THREE_OF_A_KIND
      case _ if isTwoPair(hand)      => HandType.TWO_PAIR
      case _ if isOnePair(hand)      => HandType.ONE_PAIR
      case default                   => HandType.HIGH_CARD
  }

  def getBestPossibleHandType(hand: String) :HandType = {
    var bestPossibleHandType = getHandType(hand)

    cards.foreach(c => {
      val candidate = hand.replace('J', c)
      val handType = getHandType(candidate)
      if (handType.ordinal > bestPossibleHandType.ordinal) bestPossibleHandType = handType
    })

    bestPossibleHandType
  }

  def isFiveOfAKind(hand: String): Boolean = hand.forall(_ == hand.head)

  def isFourOfAKind(hand: String): Boolean =
    hand.distinct.exists(ch => hand.count(_ == ch) == 4)

  def isFullHouse(hand: String): Boolean = {
    val groupedChars = hand.groupBy(identity).values.toList
    groupedChars.exists(group => group.length == 2) && groupedChars.exists(
      group => group.length == 3
    )
  }

  def isThreeOfAKind(hand: String): Boolean =
    hand.distinct.exists(ch => hand.count(_ == ch) == 3)

  def isTwoPair(hand: String): Boolean = {
    val groupedChars = hand.groupBy(identity).values.toList
    groupedChars.count(group => group.length == 2) == 2
  }

  def isOnePair(hand: String): Boolean = {
    val groupedChars = hand.groupBy(identity).values.toList
    groupedChars.exists(group => group.length == 2)
  }

  def isHighCard(hand: String): Boolean = hand.distinct.length == 5

  def findHighestCard(hand: String): Char = {
    var highestCardIndex = -1
    for (c <- hand)
      if (cards.indexOf(c) > highestCardIndex)
        highestCardIndex = cards.indexOf(c)

    cards(highestCardIndex)
  }

  def parseInput(line: String) = {
    line
      .slice(line.indexOf(":") + 1, line.length)
      .split("\\s+")
      .filter(_.nonEmpty)
      .map(t => t.trim().toInt)
      .toList
  }
}
