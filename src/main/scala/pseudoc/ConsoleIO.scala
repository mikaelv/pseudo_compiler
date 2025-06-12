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

// Web implementation for browser usage
case class WebConsoleIO(output: String = "", inputQueue: List[String] = List.empty) extends ConsoleIO {
  def print(text: String): ConsoleIO = {
    WebConsoleIO(output + text, inputQueue)
  }

  def readLine(): (String, ConsoleIO) = {
    inputQueue match {
      case head :: tail =>
        // Use pre-provided input from queue
        (head, WebConsoleIO(output, tail))
      case Nil =>
        // Prompt user for input via browser prompt
        try {
          import org.scalajs.dom.window
          val input = Option(window.prompt("Please enter input:")).getOrElse("")
          (input, this)
        } catch {
          case _: Exception => ("", this) // Fallback if prompt fails
        }
    }
  }

  def getOutput: String = output
  
  // Method to pre-populate input for batch processing
  def withInput(inputs: String*): WebConsoleIO = {
    WebConsoleIO(output, inputs.toList)
  }
}