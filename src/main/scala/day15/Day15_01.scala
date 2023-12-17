package day15

import utils.FileReader

class Day15_01 {
  def solve(): String = {
    val lines: List[String] =
      new FileReader().readLinesAsList("src/main/scala/day15/input.txt")
    val inputs = lines(0)
      .split(",")
      .map(_.trim())
      .toList

    val results: List[Int] = inputs.map(calculateHash)
    results.sum.toString()
  }

  def calculateHash(input: String): Int = {
    var currentValue = 0
    for (c <- input) {
      currentValue += c.toInt
      currentValue *= 17
      currentValue %= 256
    }

    return currentValue
  }
}
