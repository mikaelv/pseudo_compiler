package pseudoc

import pseudoc.ast.*
import pseudoc.ast.{ConsoleOutput, DefaultConsoleOutput}

// Result of evaluation containing both console output and updated variables
case class EvalResult(console: ConsoleOutput, vars: Map[String, Any])

object PseudoInterpreter {
  // For backward compatibility
  def eval(stmt: Statement, vars: Map[String, Any], console: ConsoleOutput = DefaultConsoleOutput()): ConsoleOutput = {
    evalWithVars(stmt, vars, console).console
  }
  
  // New evaluation method that returns both console output and updated variables
  def evalWithVars(stmt: Statement, vars: Map[String, Any], console: ConsoleOutput = DefaultConsoleOutput()): EvalResult = {
    stmt match {
      case f: ForLoop =>
        val result = (f.start to f.end).foldLeft(EvalResult(console, vars)) { (current, i) =>
          val loopVars = current.vars + (f.variable -> i)
          f.statements.foldLeft(EvalResult(current.console, loopVars)) { (res, s) =>
            evalWithVars(s, res.vars, res.console)
          }
        }
        result

      case f @ FunctionCall("print", args) =>
        val arg0: Expression[String] = args.head.asInstanceOf[Expression[String]]
        EvalResult(console.print(evalExpr(arg0, vars)), vars)

      case FunctionCall(fnName, args) => ???
        
      case ifStmt: IfStatement =>
        if (evalBoolExpr(ifStmt.condition, vars)) {
          ifStmt.thenBranch.foldLeft(EvalResult(console, vars)) { (res, s) => 
            evalWithVars(s, res.vars, res.console)
          }
        } else if (ifStmt.elseBranch.isDefined) {
          ifStmt.elseBranch.get.foldLeft(EvalResult(console, vars)) { (res, s) => 
            evalWithVars(s, res.vars, res.console)
          }
        } else {
          EvalResult(console, vars)
        }
        
      case strAssign: StringAssignment =>
        // Check if variable exists
        if (!vars.contains(strAssign.variable)) {
          throw new RuntimeException(s"Variable '${strAssign.variable}' is not declared")
        }
        
        // Check if variable is a string type
        if (!vars(strAssign.variable).isInstanceOf[String]) {
          throw new RuntimeException(s"Type error: Cannot assign string value to non-string variable '${strAssign.variable}'")
        }
        
        // Evaluate the string expression
        val stringValue = evalExpr(strAssign.value, vars)
        
        // Return with updated variable map
        EvalResult(console, vars + (strAssign.variable -> stringValue))
        
      case intAssign: IntAssignment =>
        // Check if variable exists
        if (!vars.contains(intAssign.variable)) {
          throw new RuntimeException(s"Variable '${intAssign.variable}' is not declared")
        }
        
        // Check if variable is an int type
        if (!vars(intAssign.variable).isInstanceOf[Int]) {
          throw new RuntimeException(s"Type error: Cannot assign integer value to non-integer variable '${intAssign.variable}'")
        }
        
        val intValue = evalIntExpr(intAssign.value, vars)
        EvalResult(console, vars + (intAssign.variable -> intValue))
    }
  }

  def evalBoolExpr(expr: BooleanExpression, vars: Map[String, Any]): Boolean = {
    expr match {
      case Comparison(left, op, right) => 
        val leftVal = evalIntExpr(left, vars)
        val rightVal = evalIntExpr(right, vars)
        
        op match {
          case ComparisonOperator.Equal => leftVal == rightVal
          case ComparisonOperator.NotEqual => leftVal != rightVal
          case ComparisonOperator.LessThan =>
            // TODO why .toDouble ? Claude FFS, missed it
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

  // TODO move vars to a class with one Map per type
  def evalIntExpr(expr: Expression[Int], vars: Map[String, Any]): Int = {
    expr match {
      case IntRef(varName) => vars(varName).asInstanceOf[Int]
      case IntLiteral(value) => value
      case IntMultDiv(base, ops) =>
        ops.foldLeft(evalIntExpr(base, vars)) { case (left, (op, right)) => op match {
          case MultDivOperator.Mult => left * evalIntExpr(right, vars)
          case MultDivOperator.Div => left / evalIntExpr(right, vars)
        }
        }


      case IntAddSub(base, ops) =>
        ops.foldLeft(evalIntExpr(base, vars)) { case (left, (op, right)) => op match {
          case AddSubOperator.Add => left + evalIntExpr(right, vars)
          case AddSubOperator.Sub => left - evalIntExpr(right, vars)
        }
      }
    }
  }


  def evalExpr(expr: Expression[_], vars: Map[String, Any]): String =
    expr match
      case StringLiteral(value) => value
      case StringRef(varName)   => vars(varName).toString
      case StringConcat(values) => values.map(e => evalExpr(e, vars)).mkString
      // TODO move to a different method
      case boolExpr: BooleanExpression => evalBoolExpr(boolExpr, vars).toString
}