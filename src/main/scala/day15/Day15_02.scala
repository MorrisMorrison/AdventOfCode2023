package day15

import utils.FileReader
import scala.collection.mutable.ListBuffer
import java.util.LinkedHashMap

class Day15_02 {
  def solve(): String = {
    val lines: List[String] =
      new FileReader().readLinesAsList("src/main/scala/day15/input.txt")
    val inputs = lines(0)
      .split(",")
      .map(_.trim())
      .toList

    var boxes: LinkedHashMap[Int, List[String]] = LinkedHashMap()
    (0 until 256).foreach(i => boxes.put(i, List[String]()))
    
    inputs.foreach(input => {
      var hash = -1
      var label = ""

      if (input.contains("-")) {
        label = input.split("-")(0).trim()
        hash = calculateHash(label)

        boxes.put(hash, boxes.get(hash).filter(!_.contains(label)))
      }

      if (input.contains("=")) {
        label = input.split("=")(0).trim()
        hash = calculateHash(label)
        if (!boxes.get(hash).exists(_.contains(label))) {
          boxes.put(hash, boxes.get(hash) :+ input)
        }else{
          boxes.put(hash, boxes.get(hash).updated(boxes.get(hash).indexWhere(_.contains(label)), input))
        }
      }

    })

    calculateFocusPower(boxes).toString()
  }

  def calculateFocusPower(boxes: LinkedHashMap[Int, List[String]]): Int = {
    var result = 0
    (0 until 256).foreach { i =>
      val box = boxes.get(i)
      if (box != null && !box.isEmpty) {
        for (j <- 0 until box.size) {
          var focusPower = (i+1) * (j + 1) * box.apply(j).split("=")(1).trim().toInt
          result+=focusPower
        }
      }
    }

    result
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
