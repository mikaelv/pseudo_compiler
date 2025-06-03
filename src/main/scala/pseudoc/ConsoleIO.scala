package pseudoc

import scala.io.StdIn

/** Immutable console output interface */
sealed trait ConsoleIO {
  def print(text: String): ConsoleIO

  /** return an empty string if the input is EOF */
  def readLine(): (String, ConsoleIO)
  def getOutput: String
}

// Default implementation that prints to console and captures output
case class DefaultConsoleIO(output: String = "") extends ConsoleIO {
  def print(text: String): ConsoleIO = {
    scala.Predef.print(text)
    DefaultConsoleIO(output + text)
  }

  def readLine(): (String, ConsoleIO) = {
    (Option(StdIn.readLine()).getOrElse(""), this)
  }

  def getOutput: String = output
}

// Test implementation that only captures output without printing to console
case class TestConsoleIO(output: String = "", input: String = "") extends ConsoleIO {

  def print(text: String): ConsoleIO = {
    TestConsoleIO(output + text)
  }

  override def readLine(): (String, ConsoleIO) =
    val idx = input.indexOf('\n')
    if (idx == -1)
      (input, this.copy(input = ""))
    else
      val firstLine = input.substring(0, idx)
      val otherLines = input.substring(idx+1)
      (firstLine, this.copy(input = otherLines))


  def getOutput: String = output
}