package day02

import utils.FileReader

class Day02_01 {
  def solve(): String = {
    val cubesConfiguration: Map[String, Int] =
      Map("red" -> 12, "green" -> 13, "blue" -> 14)
    val lines: List[String] =
      FileReader().readLinesAsList("src/main/scala/day02/input_01.txt")
    var result = 0
    for (i <- 0 until lines.length) do
      val input = lines(i);
      val gameNumber = i + 1
      val slice = input.slice(input.indexOf(":") + 2, input.length())
      var isGameValid = true
      val draws = slice.split(";")
      for (draw <- draws) do
        val steps = draw.split(",")
        for (step <- steps) do
          val input = parseInput(step)
          println(input)
          if (input._2 > cubesConfiguration(input._1))
            isGameValid = false

      if (isGameValid)
        println("Found valid game with number " + gameNumber)
        result += gameNumber

    result.toString()
  }

  def parseInput(input: String): (String, Int) = {
    val trimmedInput = input.trim()
    val parts = trimmedInput.split("\\s+")
    val color = parts(1)
    val number = parts(0).toInt

    (color, number)
  }
}
