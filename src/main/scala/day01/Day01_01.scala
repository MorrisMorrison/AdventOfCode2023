package day01

import scala.io.Source
import scala.util.boundary, boundary.break
import utils.FileReader

class Day01_01 {
  def findFirstDigit(text: String, reverse: Boolean = false): Int = {
    val range = if (reverse) text.length - 1 to 0 by -1 else 0 until text.length
    boundary:
      for i <- range do
        val currentChar = text.charAt(i)
        if (currentChar.isDigit) then break(currentChar.asDigit)

      0
  }

  def solve(): String = {
    val lines =
      FileReader().readLinesAsList("src/main/scala/day01/input_01.txt")
    lines
      .map(line => {
        val firstDigit = findFirstDigit(line)
        val lastDigit = findFirstDigit(line, reverse = true)
        (firstDigit, lastDigit)
      })
      .map { case (first, last) => s"$first$last" }
      .map(_.toInt)
      .sum
      .toString()
  }

}
