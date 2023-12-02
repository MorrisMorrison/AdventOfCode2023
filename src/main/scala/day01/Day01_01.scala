package day01

import scala.io.Source
import scala.util.boundary, boundary.break

class Day01_01 {
  def readFile(fileName: String): List[String] =
    Source.fromFile(fileName).getLines.toList

  def findFirstNumber(text: String, reverse: Boolean = false): Int = {
    val range = if (reverse) text.length - 1 to 0 by -1 else 0 until text.length
    boundary:
      for i <- range do
        if (text.charAt(i).isDigit) then break(text.charAt(i).asDigit)

      0
  }

  def solve(): String = {
    val lines = readFile("src/main/scala/day01/input_01.txt")
    lines
      .map(line => {
        val firstNumber = findFirstNumber(line)
        val lastNumber = findFirstNumber(line, true)
        (firstNumber, lastNumber)
      })
      .map { case (first, last) => s"$first$last" }
      .map(_.toInt)
      .sum
      .toString()
  }

}
