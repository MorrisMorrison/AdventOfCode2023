package day08

import utils.FileReader
import scala.util.boundary, boundary.break
import utils.Stopwatch

class Day08_01 {
  def solve() = {
    val lines: List[String] =
      new FileReader().readLinesAsList("src/main/scala/day08/input.txt")
    val instructionPattern: String = lines(0)
    val rawInstructions: List[String] = lines.slice(2, lines.length)
    val instructions = parseInstructions(rawInstructions)

    var currentInstruction: String = "AAA"
    var targetInstruction: String = "ZZZ" 

    var steps: Long  = 0
    while (currentInstruction != targetInstruction) {
      boundary {
        for (leftRightInstruction <- instructionPattern) {
          steps += 1

          if (leftRightInstruction == 'L') {
            currentInstruction = instructions(currentInstruction)._1
          } else {
            currentInstruction = instructions(currentInstruction)._2
          }


          if (currentInstruction == targetInstruction) {
            break()
          }
        }
      }
    }

    steps.toString()
  }

  def parseInstruction(line: String): (String, (String, String)) = {
    val parts = line.split("=")
    val instruction = parts(0).trim()
    val left = parts(1).replace("(", "").replace(")", "").split(",")(0).trim()
    val right = parts(1).replace("(", "").replace(")", "").split(",")(1).trim()
    (instruction, (left, right))
  }

  def parseInstructions(lines: List[String]): Map[String, (String, String)] = {
    lines
      .map(line => parseInstruction(line))
      .toMap
  }

}
