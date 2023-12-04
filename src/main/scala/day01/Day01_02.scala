package day01

import scala.io.Source
import util.control.Breaks, util.control.Breaks.breakable,
  util.control.Breaks.break
import utils.FileReader

class Day01_02 {

  def findFirstAndLastDigit(text: String): (Int, Int) = {
    val digitStrings: List[String] =
      List(
        "one",
        "two",
        "three",
        "four",
        "five",
        "six",
        "seven",
        "eight",
        "nine"
      )
    var firstDigit: Int = 0
    var lastDigit: Int = 0

    for i <- 0 until text.length() do
      val currentChar = text.charAt(i)
      breakable {
        if (currentChar.isDigit)
          lastDigit = currentChar.asDigit
          if (firstDigit == 0) firstDigit = currentChar.asDigit
          break

        val remainingString = text.substring(i)
        val digit =
          digitStrings.indexWhere(number =>
            remainingString.startsWith(number)
          ) + 1
        if (digit != 0)
          lastDigit = digit
        if (firstDigit == 0) firstDigit = digit
      }

    (firstDigit, lastDigit)
  }

  def solve(): String = {
    val lines =
      FileReader().readLinesAsList("src/main/scala/day01/input.txt")
    lines
      .map(line => {
        val firstAndLastDigit = findFirstAndLastDigit(line)
        (firstAndLastDigit._1, firstAndLastDigit._2)
      })
      .map { case (first, last) => s"$first$last" }
      .map(_.toInt)
      .sum
      .toString()
  }

}
