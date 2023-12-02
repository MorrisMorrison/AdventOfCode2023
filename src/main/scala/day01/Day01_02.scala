package day01

import scala.io.Source
import util.control.Breaks, util.control.Breaks.breakable,
  util.control.Breaks.break

class Day01_02 {
  private val numbersAsStrings: List[String] =
    List("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

  def readFile(fileName: String): List[String] =
    Source.fromFile(fileName).getLines.toList

  def findFirstAndLastNumber(text: String): (Int, Int) = {
    var firstNumber: Int = 0
    var lastNumber: Int = 0

    for i <- 0 until text.length() do
      val c = text.charAt(i)
      breakable {
        if (c.isDigit)
          lastNumber = c.asDigit
          if (firstNumber == 0) firstNumber = c.asDigit
          break

        val subString = text.substring(i)
        val index = numbersAsStrings.indexWhere(number => subString.startsWith(number))
        if (index != -1) 
          lastNumber = index + 1
        if (firstNumber == 0) firstNumber = index + 1
      }

    (firstNumber, lastNumber)
  }

  def solve(): String = {
    val lines = readFile("src/main/scala/day01/input_02.txt")
    lines
      .map(line => {
        val firstAndLastNumber = findFirstAndLastNumber(line)
        (firstAndLastNumber._1, firstAndLastNumber._2)
      })
      .map { case (first, last) => s"$first$last" }
      .map(_.toInt)
      .sum
      .toString()
  }

}
