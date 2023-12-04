package day04

import utils.FileReader
import scala.collection.mutable.Map
import scala.math._

class Day04_02 {
  def solve(): String = {
    val lines = FileReader().readLinesAsList("src/main/scala/day04/input.txt")
    // val cardAmounts = new Array[Int](lines.length)
    val cardAmounts = Array.fill(lines.length) { 1 }

    var cardIndex = 0
    lines
      .map(line => {
        val rawCard = line
          .slice(line.indexOf(':') + 1, line.length())
        val Array(left, right) = rawCard
          .split("\\|")
          .map(_.trim)
        val winners = left
          .split("\\s+")
          .map(_.trim().toInt)
          .toList
        val myNumbers = right
          .split("\\s+")
          .map(_.trim().toInt)
          .toList
        val matching = winners.intersect(myNumbers).length
        for (_ <- 0 until cardAmounts(cardIndex)) do
          for (i <- 0 until matching) do
            if (cardIndex + i + 1 < cardAmounts.length) then
              cardAmounts(cardIndex + i + 1) += 1

        cardIndex += 1
        if matching == 0 then 0
        else pow(2, matching - 1)
      })
      .sum
      .toString()

    cardAmounts.sum.toString()
  }
}
