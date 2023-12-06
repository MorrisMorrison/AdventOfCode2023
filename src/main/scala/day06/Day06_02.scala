package day06

import utils.FileReader
import scala.collection.mutable.ListBuffer
case class LongRace(time: Long, distance: Long)

class Day06_02 {
  def solve() = {
    val lines: List[String] =
      new FileReader().readLinesAsList("src/main/scala/day06/input.txt")

    val time = parseInput(lines(0))
    val distance = parseInput(lines(1))  
    val race = new LongRace(time, distance)

    val winningHoldTimes = (0L until race.time).count { holdInSeconds =>
      val resultDistance = (race.time - holdInSeconds) * holdInSeconds
      resultDistance > race.distance
    }

    winningHoldTimes
  }

  def parseInput(line: String): Long =
    line
      .slice(line.indexOf(":") + 1, line.length)
      .replaceAll("\\s", "")
      .toLong

}
