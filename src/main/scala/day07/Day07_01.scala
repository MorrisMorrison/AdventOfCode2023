package day07

import utils.FileReader

class Day07_01 {

  case class Hand(handType: HandType, cards: List[Char])
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
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9",
    "T",
    "J",
    "Q",
    "K",
    "A"
  )

  def solve() = {}

  def getHandType(hand: String): HandType = {
    hand match
      case _ if isFiveOfAKind(hand)  => HandType.FIVE_OF_A_KIND
      case _ if isFourOfAKind(hand)  => HandType.FOUR_OF_A_KIND
      case _ if isFullHouse(hand)    => HandType.FULL_HOUSE
      case _ if isThreeOfAKind(hand) => HandType.THREE_OF_A_KIND
      case _ if isTwoPair(hand)      => HandType.TWO_PAIR
      case _ if isOnePair(hand)      => HandType.ONE_PAIR
      case _ if isHighCard(hand)     => HandType.HIGH_CARD
  }

  def isFiveOfAKind(hand: String): Boolean = hand.forall(_ == hand.head)

  def isFourOfAKind(hand: String): Boolean =
    hand.distinct.length == 2 && hand.distinct.tail.forall(_ == hand.head)

  def isFullHouse(hand: String): Boolean = {
    val groupedChars = hand.groupBy(identity).values.toList
    groupedChars.length == 2 && groupedChars.exists(group =>
      group.length == 2
    ) && groupedChars.exists(group => group.length == 3)
  }

  def isThreeOfAKind(hand: String): Boolean =
    hand.distinct.length == 3 && hand.distinct.forall(ch =>
      hand.count(_ == ch) == 3
    )

  def isTwoPair(hand: String): Boolean = {
    val groupedChars = hand.groupBy(identity).values.toList
    groupedChars.length == 2 && groupedChars.forall(group => group.length == 2)
  }

  def isOnePair(hand: String): Boolean = {
    val groupedChars = hand.groupBy(identity).values.toList
    groupedChars.length == 2 && groupedChars.exists(group => group.length == 2)
  }

  def isHighCard(hand: String): Boolean = hand.distinct.length == 5

  def findHighestCard(hand: String): Char = {
    return 'A'
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
