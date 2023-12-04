package day04

import utils.FileReader
import scala.collection.mutable.Map
import scala.math._

class Day04_01 {

  def solve(): String = {
    val lines = FileReader().readLinesAsList("src/main/scala/day04/input.txt")
    
    lines
      .map(line => {
        val rawCard = line.slice(line.indexOf(':') + 1, line.length())
        val Array(left, right) = rawCard.split("\\|").map(_.trim)
        val winners = left.split("\\s+").map(_.trim().toInt).toList
        val myNumbers = right.split("\\s+").map(_.trim().toInt).toList
        val matching = winners.intersect(myNumbers).length
        if matching == 0 then 0 else pow(2, matching - 1)
      })
      .sum
      .toString()
  }
}
