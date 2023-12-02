package day02

import utils.FileReader

class Day02_02 {
  def solve(): String = {
    val cubesConfiguration: Map[String, Int] =
      Map("red" -> 12, "green" -> 13, "blue" -> 14)
    val lines: List[String] =
      FileReader().readLinesAsList("src/main/scala/day02/input.txt")
    var result = 0
    for (i <- 0 until lines.length) do
      val input = lines(i);
      var isGameValid = true
      val draws = input
        .slice(input.indexOf(":") + 2, input.length())
        .split(";")
      for (draw <- draws) do
        for (step <- draw.split(",")) do
          val input = parseInput(step)
          if (input._2 > cubesConfiguration(input._1))
            isGameValid = false

      if (isGameValid)
        result += i + 1

    result.toString()
  }

  def parseInput(input: String): (String, Int) = {
    val parts = input.trim().split("\\s+")

    (parts(1), parts(0).toInt)
  }
}
