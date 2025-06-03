package pseudoc.ast

import pseudoc.ast

import java.io.{BufferedReader, StringReader}
import scala.io.StdIn

// Immutable console output interface
trait ConsoleIO {
  def print(text: String): ConsoleIO
  // TODO return a new ConsoleIO:  more functional
  def readLine(): Option[String]
  def getOutput: String
}

// Default implementation that prints to console and captures output
case class DefaultConsoleIO(output: String = "") extends ConsoleIO {
  def print(text: String): ConsoleIO = {
    scala.Predef.print(text)
    DefaultConsoleIO(output + text)
  }

  def readLine(): Option[String] = {
    Option(StdIn.readLine())
  }

  def getOutput: String = output
}

// Test implementation that only captures output without printing to console
case class TestConsoleIO(output: String = "", input: String="") extends ConsoleIO {
  private val reader = BufferedReader(new StringReader(input))

  def print(text: String): ConsoleIO = {
    TestConsoleIO(output + text)
  }

  override def readLine(): Option[String] = {
    Option(reader.readLine())
  }

  def getOutput: String = output
}

case class Algorithm(name: String) extends Ast
// Base trait for variable declarations
sealed trait VariableDeclaration {
  def name: String
  def tpe: pseudoc.PseudoType
}

case class VariableDecl(name: String, tpe: pseudoc.PseudoType) extends VariableDeclaration
case class ArrayVariableDecl(name: String, tpe: pseudoc.PseudoType, size: Int) extends VariableDeclaration

case class Variables(vars: Seq[VariableDeclaration])
object Variables {
  def fromSeq(seq: Seq[Variables]): Variables =
    seq.foldLeft(new Variables(Seq.empty)) { case (res, vars) =>
      new ast.Variables(res.vars ++ vars.vars)
    }
}

sealed trait Ast {}

object Ast {}
