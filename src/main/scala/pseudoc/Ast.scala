package pseudoc

import pseudoc.Ast.Expression

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

  def eval(stmt: Statement, vars: Map[String, Any], console: ConsoleOutput = DefaultConsoleOutput()): ConsoleOutput = {
    stmt match {
      case f: ForLoop =>
        val finalConsole = (f.start to f.end).foldLeft(console) { (currentConsole, i) =>
          f.statements.foldLeft(currentConsole) { (c, s) =>
            eval(s, vars + (f.variable -> i), c)
          }
        }
        finalConsole

      case f @ FunctionCall("print", args) =>
        val arg0: Expression[String] =
          args.head.asInstanceOf[Expression[String]]
        console.print(evalExpr(arg0, vars))

      case FunctionCall(fnName, args) => ???
        
      case ifStmt: IfStatement =>
        if (evalBoolExpr(ifStmt.condition, vars)) {
          ifStmt.thenBranch.foldLeft(console) { (c, s) => 
            eval(s, vars, c)
          }
        } else if (ifStmt.elseBranch.isDefined) {
          ifStmt.elseBranch.get.foldLeft(console) { (c, s) => 
            eval(s, vars, c)
          }
        } else {
          console
        }
    }
  }

  def evalBoolExpr(expr: BooleanExpression, vars: Map[String, Any]): Boolean = {
    expr match {
      case Comparison(left, op, right) => 
        val leftVal = evalExpr(left, vars)
        val rightVal = evalExpr(right, vars)
        
        op match {
          case ComparisonOperator.Equal => leftVal == rightVal
          case ComparisonOperator.NotEqual => leftVal != rightVal
          case ComparisonOperator.LessThan => 
            leftVal.toDouble < rightVal.toDouble
          case ComparisonOperator.GreaterThan => 
            leftVal.toDouble > rightVal.toDouble
          case ComparisonOperator.LessThanEqual => 
            leftVal.toDouble <= rightVal.toDouble
          case ComparisonOperator.GreaterThanEqual => 
            leftVal.toDouble >= rightVal.toDouble
        }
    }
  }

  def evalExpr(expr: Expression[_], vars: Map[String, Any]): String =
    expr match
      case StringLiteral(value) => value
      case StringRef(varName)   => vars(varName).toString
      case IntRef(varName)      => vars(varName).toString
      case IntLiteral(value)    => value.toString
      case StringConcat(values) => values.map(e => evalExpr(e, vars)).mkString
      case boolExpr: BooleanExpression => evalBoolExpr(boolExpr, vars).toString
}