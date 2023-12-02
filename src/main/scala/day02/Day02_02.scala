package day02

import utils.FileReader
import scala.collection.mutable.Map
class Day02_02 {
  def solve(): String = {
    val lines: List[String] =
      FileReader().readLinesAsList("src/main/scala/day02/input.txt")
    var result = 0
    for (i <- 0 until lines.length) do
      val minimalRequiredCubeAmountByColor: Map[String, Int] = Map("red" -> 0, "green" -> 0, "blue" -> 0)
      val line = lines(i);
      val draws = line 
        .slice(line.indexOf(":") + 2, line.length())
        .split(";")
      for (draw <- draws) do
        for (step <- draw.split(",")) do
          val input = parseInput(step)
          if (minimalRequiredCubeAmountByColor(input._1) < input._2)
            minimalRequiredCubeAmountByColor(input._1) = input._2

      result += minimalRequiredCubeAmountByColor("red") * minimalRequiredCubeAmountByColor("green") * minimalRequiredCubeAmountByColor("blue")
    
    result.toString()
  }

  def parseInput(input: String): (String, Int) = {
    val parts = input.trim().split("\\s+")
    (parts(1), parts(0).toInt)
  }
}
