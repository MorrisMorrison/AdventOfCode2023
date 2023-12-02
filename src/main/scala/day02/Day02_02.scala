package day02

import utils.FileReader
import scala.collection.mutable.Map
class Day02_02 {
  def solve(): String = {
    val lines: List[String] =
      FileReader().readLinesAsList("src/main/scala/day02/input.txt")
    var result = 0
    for (i <- 0 until lines.length) do
      val cubesConfiguration: Map[String, Int] = Map("red" -> 0, "green" -> 0, "blue" -> 0)
      val input = lines(i);
      val draws = input
        .slice(input.indexOf(":") + 2, input.length())
        .split(";")
      for (draw <- draws) do
        for (step <- draw.split(",")) do
          val input = parseInput(step)
          if (cubesConfiguration(input._1) < input._2)
            cubesConfiguration(input._1) = input._2
      result += cubesConfiguration("red") * cubesConfiguration("green") * cubesConfiguration("blue")
    result.toString()
  }

  def parseInput(input: String): (String, Int) = {
    val parts = input.trim().split("\\s+")

    (parts(1), parts(0).toInt)
  }
}
