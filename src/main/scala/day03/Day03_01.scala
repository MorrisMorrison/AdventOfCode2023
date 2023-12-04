package day03

import utils.FileReader
import scala.util.control.Breaks._

class Day03_01 {
  def parseInputAsMatrix(): Array[Array[Char]] =
    FileReader()
      .readLinesAsArray("src/main/scala/day03/input.txt")
      .map(_.toCharArray())

  def solve(): String = {
    var result = 0
    val matrix = parseInputAsMatrix()
    for (i <- 0 until matrix.length) do
      var j = 0
      while (j < matrix(0).length) do
        val char = matrix(i)(j)
        if (char.isDigit) {
          val numberWithEndIndex: (Int, Int) = findNumber(matrix, i, j)
          val number = numberWithEndIndex._1
          var isPartNumber = false
          for (k <- j until numberWithEndIndex._2) do
            if (hasNearbySymbol(matrix, i, k)) then
                isPartNumber = true

          if (isPartNumber) then result += number
          j = numberWithEndIndex._2
        }

        j += 1

    result.toString()
  }

  def matrix2String[T](matrix: Array[Array[T]]): String =
    matrix.map(_.mkString(" ")).mkString("\n")

  def isValidSymbol(char: Char): Boolean = {
    val asciiCode = char.toInt
    if (asciiCode == 46) then return false

    (asciiCode >= 33 && asciiCode <= 45) || (asciiCode >= 58 && asciiCode <= 64) || asciiCode == 47
  }

  def hasNearbySymbol(
      matrix: Array[Array[Char]],
      row: Int,
      col: Int
  ): Boolean = {
    val positionsToCheck = Seq(
      (row, col - 1),
      (row - 1, col - 1),
      (row + 1, col - 1),
      (row - 1, col),
      (row + 1, col),
      (row, col + 1),
      (row - 1, col + 1),
      (row + 1, col + 1)
    )

    positionsToCheck.exists { case (r, c) =>
      r >= 0 && r < matrix.length && c >= 0 && c < matrix(
        0
      ).length && isValidSymbol(matrix(r)(c))
    }
  }
  def findNumber(
      matrix: Array[Array[Char]],
      currentRow: Int,
      currentCol: Int
  ): (Int, Int) = {
    var endIndex = 0
    breakable {
      for (k <- currentCol until matrix(0).length ) do
        if ( k+1 >= matrix(currentRow).length || !matrix(currentRow)(k+1).isDigit) then
          endIndex = k+1
          break()
    }

    val number = matrix(currentRow).slice(currentCol, endIndex).mkString.toInt
    (number, endIndex)
  
}
}
