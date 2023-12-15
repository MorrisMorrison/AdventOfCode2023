package utils

class Stopwatch {
  private var startTime: Long = 0
  private var endTime: Long = 0

  def start(): Unit = {
    startTime = System.currentTimeMillis()
  }

  def stop(): Unit = {
    endTime = System.currentTimeMillis()
  }

  def elapsedTime(): Long = {
    if (startTime == 0) {
      throw new IllegalStateException("Stopwatch has not been started.")
    }
    if (endTime == 0) {
      throw new IllegalStateException("Stopwatch has not been stopped.")
    }
    endTime - startTime
  }
}
