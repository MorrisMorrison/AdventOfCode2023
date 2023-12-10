package day07

import utils.FileReader
import scala.collection.mutable.ListBuffer

class Day07_02 {
  def solve() = {
  }

  def parseInput(line: String): Long =
    line
      .slice(line.indexOf(":") + 1, line.length)
      .replaceAll("\\s", "")
      .toLong

}
