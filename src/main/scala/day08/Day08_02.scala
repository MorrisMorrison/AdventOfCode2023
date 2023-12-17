package day08

import utils.FileReader
import scala.util.boundary, boundary.break
import utils.Stopwatch
import scala.collection.mutable.ListBuffer

class Day08_02 {
  def solve() = {
    val lines: List[String] =
      new FileReader().readLinesAsList("src/main/scala/day08/input.txt")
    val leftRightInstructionPattern: String = lines(0)

    val rawInstructions: List[String] = lines.slice(2, lines.length)
    val node2LeftRightInstruction = parseInstructions(rawInstructions)

    var startNodes: List[String] = node2LeftRightInstruction
      .filter { case (node, _) => node.endsWith("A") }
      .map { case (node, _) => node }
      .toList

    var nodes2steps = Map[String, Int]()
    for (startNode <- startNodes) {
      var steps: Long = 0
      var currentNode = startNode
      while (!isTargetNode(currentNode)) {
        boundary {
          for (leftRightInstruction <- leftRightInstructionPattern) {
            steps += 1
            if (leftRightInstruction == 'L') {
              currentNode = node2LeftRightInstruction(currentNode)._1
            } else {
              currentNode = node2LeftRightInstruction(currentNode)._2
            }
          }

          if (isTargetNode(currentNode)) {
            break()
          }
        }
      }

      nodes2steps += (startNode -> steps.toInt)
    }

    calculateLCM(nodes2steps.values.toList).toString()
  }

  def parseInstruction(line: String): (String, (String, String)) = {
    val parts = line.split("=")
    val node = parts(0).trim()
    val left = parts(1).replace("(", "").replace(")", "").split(",")(0).trim()
    val right = parts(1).replace("(", "").replace(")", "").split(",")(1).trim()
    (node, (left, right))
  }

  def parseInstructions(lines: List[String]): Map[String, (String, String)] = {
    lines
      .map(line => parseInstruction(line))
      .toMap
  }

  def isTargetNode(node: String): Boolean = {
    node.endsWith("Z")
  }

  def calculateLCM(numbers: List[Int]): Long = {
    def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)
    def lcm(a: Long, b: Long): Long = Math.abs(a * b) / gcd(a, b)

    numbers.foldLeft(1L)((x, y) => lcm(x, y))
  }
}
