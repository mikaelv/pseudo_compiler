package pseudoc

import pseudoc.Ast._

object PseudoInterpreter {
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