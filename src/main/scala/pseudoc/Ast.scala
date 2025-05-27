package pseudoc

import pseudoc.Ast.Expression

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

  def eval(stmt: Statement, vars: Map[String, Any]): Unit = {
    stmt match {
      case f: ForLoop =>
        for (i <- f.start to f.end) {
          f.statements.foreach(s => eval(s, vars + (f.variable -> i)))
        }

      case f @ FunctionCall("print", args) =>
        val arg0: Expression[String] =
          args.head.asInstanceOf[Expression[String]]
        print(evalExpr(arg0, vars))

      case FunctionCall(fnName, args) => ???
        
      case ifStmt: IfStatement =>
        if (evalBoolExpr(ifStmt.condition, vars)) {
          ifStmt.thenBranch.foreach(s => eval(s, vars))
        } else if (ifStmt.elseBranch.isDefined) {
          ifStmt.elseBranch.get.foreach(s => eval(s, vars))
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