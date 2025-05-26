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
  sealed trait ExpressionItem[A]
  case class StringLiteral(value: String) extends ExpressionItem[String]
  case class StringRef(varName: String) extends ExpressionItem[String]
  case class IntRef(varName: String) extends ExpressionItem[Int]

  sealed trait Expression[A]
  case class StringConcat(values: Seq[ExpressionItem[_]])
      extends Expression[String]
  case class FunctionCall(fnName: String, args: Seq[Expression[_]])
      extends Statement

  def eval(stmt: Statement, vars: Map[String, Any]): Unit = {
    stmt match {
      case f: ForLoop =>
        for (i <- f.start to f.end) {
          f.statements.foreach(s => eval(s, vars + (f.variable -> i)))
        }

      case f@FunctionCall("print", args) =>
        val arg0: Expression[String] = args.head.asInstanceOf[Expression[String]]
        print(evalExpr(arg0, vars))

      case FunctionCall(fnName, args) => ???

    }

  }

  def evalExpr(expression: Expression[String], vars: Map[String, Any]): String =
    expression match
      case StringConcat(values) =>
        values.map(e => evalString(e, vars)).mkString

  def evalString(item: ExpressionItem[_], vars: Map[String, Any]): String =
    item match
      case StringLiteral(value) => value.replaceAll("\\\\NL", "\n")
      case StringRef(varName)   => vars(varName).toString
      case IntRef(varName)      => vars(varName).toString
}
