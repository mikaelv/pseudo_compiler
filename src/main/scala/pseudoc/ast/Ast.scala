package pseudoc.ast

import pseudoc.ast

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
