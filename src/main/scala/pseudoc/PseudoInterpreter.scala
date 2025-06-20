package pseudoc

import pseudoc.ast.*
import pseudoc.PseudoType

// Result of evaluation containing both console output and updated variables
case class EvalResult(console: ConsoleIO, vars: VarMap)

object PseudoInterpreter {

  /** Parse, type check, and evaluate a program from source code
    *
    * @param source
    *   The program source code
    * @param console
    *   Console output implementation
    * @return
    *   Either an error message or the evaluation result
    */
  def run(
      source: String,
      console: ConsoleIO = DefaultConsoleIO()
  ): Either[String, EvalResult] = {
    // Parse the program
    PseudoCodeParser.parseProgram(source) match {
      case Left(parseError) =>
        Left(s"Parse error: $parseError")

      case Right(program) =>
        // Type check the program
        program.typeCheck() match {
          case Left(typeError) =>
            Left(s"Type error: $typeError")

          case Right(_) =>
            // Initialize empty variable map
            val initialVarMap = createVarMapFromDeclarations(program.variables)

            // Evaluate each statement
            val result = program.statements.foldLeft(EvalResult(console, initialVarMap)) {
              (result, statement) => evalStmt(statement, result.vars, result.console)
            }

            Right(result)
        }
    }
  }

  /** Create a VarMap from variable declarations
    */
  private[pseudoc] def createVarMapFromDeclarations(variables: Variables): VarMap = {
    val initialValues = variables.vars.map {
      case VariableDecl(name, tpe) =>
        val initialValue = tpe match {
          case PseudoType.StringType => ""
          case PseudoType.IntType    => 0
          case PseudoType.BoolType   => false
          case PseudoType.ArrayIntType =>
            throw new RuntimeException(
              s"Regular VariableDecl should not have ArrayIntType. Use ArrayVariableDecl instead."
            )
        }
        (name, initialValue)

      case ArrayVariableDecl(name, tpe, size) =>
        val initialValue = tpe match {
          case PseudoType.ArrayIntType =>
            Array.fill(size)(0) // Create array with specified size, filled with zeros
          case _ => throw new RuntimeException(s"ArrayVariableDecl used with non-array type: $tpe")
        }
        (name, initialValue)
    }

    VarMap(initialValues: _*)
  }
  // For backward compatibility
  def eval(
      stmt: Statement,
      vars: VarMap,
      console: ConsoleIO = DefaultConsoleIO()
  ): ConsoleIO = {
    evalStmt(stmt, vars, console).console
  }

  // TODO call from PseudoInterpreterTest
  /*def evalProgram(program: Program, console: ConsoleOutput = DefaultConsoleOutput()): EvalResult = {
    val vars = VarMap.empty // TODO initialize from declarations

    program.statements.foldLeft(EvalResult(console, vars)) { case (res, stmt) =>
      evalWithVars(stmt, res.vars, res.console)
    }
  }*/

  // New evaluation method that returns both console output and updated variables
  def evalStmt(
      stmt: Statement,
      vars: VarMap,
      console: ConsoleIO = DefaultConsoleIO()
  ): EvalResult = {
    stmt match {
      case f: ForLoop =>
        val result = (f.start.value to f.end.value).foldLeft(EvalResult(console, vars)) {
          (current, i) =>
            val loopVars = current.vars.store(f.variable, i)
            f.statements.foldLeft(EvalResult(current.console, loopVars)) { (res, s) =>
              evalStmt(s, res.vars, res.console)
            }
        }
        result

      case w: WhileLoop =>
        var currentResult = EvalResult(console, vars)
        while (evalBoolExpr(w.condition, currentResult.vars)) {
          currentResult = w.statements.foldLeft(currentResult) { (res, s) =>
            evalStmt(s, res.vars, res.console)
          }
        }
        currentResult

      case f @ FunctionCall("print", args) =>
        val str = args.foldLeft("") { case (res, expr) =>
          res + evalExpr(expr, vars)
        }
        EvalResult(console.print(str), vars)

      case f @ FunctionCall("read", Seq(ref: VariableRef[_])) =>
        val (line, newConsole) = console.readLine()
        val value = ref match {
          case _: StringRef => line
          case _: IntRef    => line.trim.toInt
          case _: BoolRef   => line.trim.toBoolean
        }
        EvalResult(newConsole, vars.store(ref.varName, value))

      case f: FunctionCall => throw new UnsupportedOperationException(s"Unsupported: $f")

      case ifStmt: IfStatement =>
        if (evalBoolExpr(ifStmt.condition, vars))
          ifStmt.thenBranch.foldLeft(EvalResult(console, vars)) { (res, s) =>
            evalStmt(s, res.vars, res.console)
          }
        else if (ifStmt.elseBranch.isDefined)
          ifStmt.elseBranch.get.foldLeft(EvalResult(console, vars)) { (res, s) =>
            evalStmt(s, res.vars, res.console)
          }
        else
          EvalResult(console, vars)

      case assign: Assignment => EvalResult(console, evalAssign(assign, vars))

    }
  }

  def evalExpr(expr: Expression, vars: VarMap): Any = {
    expr match {
      case a: ArrayExpression  => evalArrayExpr(a, vars)
      case b: BoolExpression   => evalBoolExpr(b, vars)
      case i: IntExpression    => evalIntExpr(i, vars)
      case s: StringExpression => evalStringExpr(s, vars)
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

  def evalIntExpr(expr: IntExpression, vars: VarMap): Int =
    expr match
      case IntRef(varName)   => vars(varName).asInstanceOf[Int]
      case IntLiteral(value) => value
      case ArrayAccess(arrayExpr, indexExpr) =>
        val array = evalArrayExpr(arrayExpr, vars)
        val index = evalIntExpr(indexExpr, vars)
        array(index - 1)  // Convert from 1-based to 0-based indexing
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

  def evalArrayExpr(expr: ArrayExpression, vars: VarMap): Array[Int] =
    expr match
      case ArrayRef(varName) =>
        val value = vars(varName)
        value match {
          case arr: Array[Int] => arr
          case _ =>
            throw new RuntimeException(
              s"Variable '$varName' is not an array, found ${value.getClass}"
            )
        }
      case ArrayLiteral(values) => values.map(evalIntExpr(_, vars)).toArray

  def evalAssign(assign: Assignment, vars: VarMap): VarMap = {
    // Check if variable exists
    if (!vars.contains(assign.variable))
      throw new RuntimeException(s"Variable '${assign.variable}' is not declared")

    // Evaluate the expression based on its type
    val value = assign.value match {
      case expr: StringExpression => evalStringExpr(expr, vars)
      case expr: IntExpression    => evalIntExpr(expr, vars)
      case expr: BoolExpression   => evalBoolExpr(expr, vars)
      case expr: ArrayExpression  => evalArrayExpr(expr, vars)
    }

    vars.store(assign.variable, value)
  }

}
