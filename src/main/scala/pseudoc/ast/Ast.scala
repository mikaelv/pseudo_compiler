package pseudoc.ast

// Immutable console output interface
trait ConsoleOutput {
  def print(text: String): ConsoleOutput
  def getOutput: String
}

// Default implementation that prints to console and captures output
case class DefaultConsoleOutput(output: String = "") extends ConsoleOutput {
  def print(text: String): ConsoleOutput = {
    scala.Predef.print(text)
    DefaultConsoleOutput(output + text)
  }
  
  def getOutput: String = output
}

// Test implementation that only captures output without printing to console
case class TestConsoleOutput(output: String = "") extends ConsoleOutput {
  def print(text: String): ConsoleOutput = {
    TestConsoleOutput(output + text)
  }
  
  def getOutput: String = output
}

case class Algorithm(name: String) extends Ast
case class Variables(vars: Seq[VariableDecl])
case class VariableDecl(name: String, tpe: String)


sealed trait Ast {}

object Ast {



}