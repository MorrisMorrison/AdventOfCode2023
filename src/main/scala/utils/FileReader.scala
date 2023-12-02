package utils
import scala.io.Source

class FileReader {
    def readLinesAsList(filePath:String) : List[String] = Source.fromFile(filePath).getLines.toList
    def readLinesAsArray(filePath:String) : Array[String] = Source.fromFile(filePath).getLines.toArray
    def readFileAsString(filePath:String): String = Source.fromFile(filePath).mkString
}
