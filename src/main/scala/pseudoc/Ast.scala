package pseudoc

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

sealed trait Ast {}

object Ast {
  sealed trait Statement

  case class Algorithm(name: String) extends Ast
  case class Variables(vars: Seq[VariableDecl])
  case class VariableDecl(name: String, tpe: String)
  case class ForLoop(
      variable: String,
      start: Int,
      end: Int,
      statements: Seq[Statement]
  ) extends Statement
  
  case class IfStatement(
      condition: BooleanExpression,
      thenBranch: Seq[Statement],
      elseBranch: Option[Seq[Statement]] = None
  ) extends Statement
  
  // Typed assignment to enforce type safety at compile time
  sealed trait Assignment extends Statement {
    def variable: String
  }
  
  case class StringAssignment(
      variable: String,
      value: Expression[String]
  ) extends Assignment
  
  case class IntAssignment(
      variable: String,
      value: Expression[Int]
  ) extends Assignment

  sealed trait Expression[A]
  case class StringLiteral(value: String) extends Expression[String]
  case class StringRef(varName: String) extends Expression[String]
  case class IntRef(varName: String) extends Expression[Int]
  case class IntLiteral(value: Int) extends Expression[Int]

  enum ComparisonOperator:
    case Equal, NotEqual, LessThan, GreaterThan, LessThanEqual, GreaterThanEqual
  
  sealed trait BooleanExpression extends Expression[Boolean]
  case class Comparison(left: Expression[_], op: ComparisonOperator, right: Expression[_]) extends BooleanExpression

  case class StringConcat(values: Seq[Expression[_]]) extends Expression[String]
  case class FunctionCall(fnName: String, args: Seq[Expression[_]])
      extends Statement
}