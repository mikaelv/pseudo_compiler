package pseudoc

import pseudoc.ast.*
import pseudoc.ast.{ConsoleOutput, DefaultConsoleOutput}

// Result of evaluation containing both console output and updated variables
case class EvalResult(console: ConsoleOutput, vars: VarMap)

object PseudoInterpreter {
  // For backward compatibility
  def eval(
      stmt: Statement,
      vars: VarMap,
      console: ConsoleOutput = DefaultConsoleOutput()
  ): ConsoleOutput = {
    evalWithVars(stmt, vars, console).console
  }

  // New evaluation method that returns both console output and updated variables
  def evalWithVars(
      stmt: Statement,
      vars: VarMap,
      console: ConsoleOutput = DefaultConsoleOutput()
  ): EvalResult = {
    stmt match {
      case f: ForLoop =>
        val result = (f.start to f.end).foldLeft(EvalResult(console, vars)) { (current, i) =>
          val loopVars = current.vars.store(f.variable, i)
          f.statements.foldLeft(EvalResult(current.console, loopVars)) { (res, s) =>
            evalWithVars(s, res.vars, res.console)
          }
        }
        result

      case f @ FunctionCallString("print", args) =>
        val arg0: StringExpression = args.head
        EvalResult(console.print(evalStringExpr(arg0, vars)), vars)

      case f: FunctionCallString => ???

      case ifStmt: IfStatement =>
        if (evalBoolExpr(ifStmt.condition, vars))
          ifStmt.thenBranch.foldLeft(EvalResult(console, vars)) { (res, s) =>
            evalWithVars(s, res.vars, res.console)
          }
        else if (ifStmt.elseBranch.isDefined)
          ifStmt.elseBranch.get.foldLeft(EvalResult(console, vars)) { (res, s) =>
            evalWithVars(s, res.vars, res.console)
          }
        else
          EvalResult(console, vars)

      case assign: Assignment => EvalResult(console, evalAssign(assign, vars))

    }
  }

  def evalBoolExpr(expr: BoolExpression, vars: VarMap): Boolean = {
    expr match
      case BoolLiteral(b) => b

      case BoolOperations(base, ops: _*) =>
        ops.foldLeft(evalBoolExpr(base, vars)):
          case (left, (op, right)) =>
            op match
              case BooleanOperator.And => left && evalBoolExpr(right, vars)
              case BooleanOperator.Or  => left || evalBoolExpr(right, vars)

      case BoolRef(varName) => vars(varName).asInstanceOf[Boolean]

      case Comparison(left, op, right) =>
        val leftVal = evalIntExpr(left, vars)
        val rightVal = evalIntExpr(right, vars)
        op match
          case ComparisonOperator.Equal            => leftVal == rightVal
          case ComparisonOperator.NotEqual         => leftVal != rightVal
          case ComparisonOperator.LessThan         => leftVal < rightVal
          case ComparisonOperator.GreaterThan      => leftVal > rightVal
          case ComparisonOperator.LessThanEqual    => leftVal <= rightVal
          case ComparisonOperator.GreaterThanEqual => leftVal >= rightVal
  }

  // TODO move vars to a class with one Map per type
  def evalIntExpr(expr: IntExpression, vars: VarMap): Int =
    expr match
      case IntRef(varName)   => vars(varName).asInstanceOf[Int]
      case IntLiteral(value) => value
      case IntMultDiv(base, ops) =>
        ops.foldLeft(evalIntExpr(base, vars)) { case (left, (op, right)) =>
          op match
            case MultDivOperator.Mult => left * evalIntExpr(right, vars)
            case MultDivOperator.Div  => left / evalIntExpr(right, vars)
        }

      case IntAddSub(base, ops) =>
        ops.foldLeft(evalIntExpr(base, vars)) { case (left, (op, right)) =>
          op match
            case AddSubOperator.Add => left + evalIntExpr(right, vars)
            case AddSubOperator.Sub => left - evalIntExpr(right, vars)
        }

  def evalStringExpr(expr: StringExpression, vars: VarMap): String =
    expr match
      case StringLiteral(value) => value
      case StringRef(varName)   => vars(varName).toString
      case StringConcat(values) =>
        values.map(e => evalStringExpr(e, vars)).mkString

  def evalAssign(assign: Assignment, vars: VarMap): VarMap = {
    // Check if variable exists
    if (!vars.contains(assign.variable))
      throw new RuntimeException(s"Variable '${assign.variable}' is not declared")

    val value = assign match
      case StringAssignment(_, expr) => evalStringExpr(expr, vars)
      case IntAssignment(_, expr)    => evalIntExpr(expr, vars)
      case BoolAssignment(_, expr)   => evalBoolExpr(expr, vars)

    vars.store(assign.variable, value)
  }

}
