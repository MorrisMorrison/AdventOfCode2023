package day06

import utils.FileReader
import scala.collection.mutable.ListBuffer

case class Race(time: Int, distance: Int)

class Day06_01 {
  def solve() = {
    val lines: List[String] =
      new FileReader().readLinesAsList("src/main/scala/day06/input.txt")

    val races = parseInput(lines(0))
      .zip(parseInput(lines(1)))
      .map { case (time, distance) =>
        Race(time, distance)
      }

    val result: List[Int] = races.map { race =>
      val winningHoldTimes = (0 until race.time).count { holdInSeconds =>
        val resultDistance = (race.time - holdInSeconds) * holdInSeconds
        resultDistance > race.distance
      }
      winningHoldTimes
    }.toList

    result.product
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

