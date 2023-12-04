package day04

import utils.FileReader
import scala.collection.mutable.Map
import scala.math._

class Day04_01 {

  def solve(): String = {
    val lines = FileReader().readLinesAsList("src/main/scala/day04/input.txt")
    
    lines
      .map(s => s.slice(s.indexOf(':') + 1, s.length()))
      .map(s => {
        val Array(left, right) = s.split("\\|").map(_.trim)
        val winners = left.split("\\s+").map(_.trim().toInt).toList
        val myNumbers = right.split("\\s+").map(_.trim().toInt).toList
        winners.intersect(myNumbers).length
      })
      .toList
      .map(result => if result == 0 then 0 else pow(2, result - 1))
      .sum
      .toString()
  }
}
