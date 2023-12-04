package day04

import utils.FileReader
import scala.collection.mutable.Map
import scala.math._

class Day04_02 {
  def solve(): String = {
    val lines = FileReader().readLinesAsList("src/main/scala/day04/input.txt")
    val cardAmounts = Array.fill(lines.length) { 1 }
    
    lines.zipWithIndex.foreach { case (line, cardIndex) =>
      val rawCard = line.slice(line.indexOf(':') + 1, line.length())
      val Array(left, right) = rawCard.split("\\|").map(_.trim)
      val winners = left.split("\\s+").map(_.trim.toInt).toList
      val myNumbers = right.split("\\s+").map(_.trim.toInt).toList
      val matching = winners.intersect(myNumbers).length

      (0 until cardAmounts(cardIndex)).foreach { _ =>
        (0 until matching).foreach { i =>
          val nextCardIndex = cardIndex + i + 1
          if (nextCardIndex < cardAmounts.length) {
            cardAmounts(nextCardIndex) += 1
          }

          None
        }
      }
    }

    cardAmounts.sum.toString()
  }

}
