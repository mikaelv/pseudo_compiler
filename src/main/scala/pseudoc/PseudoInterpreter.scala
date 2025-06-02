package pseudoc

import pseudoc.ast.{BoolExpression, ConsoleOutput, DefaultConsoleOutput, *}
import pseudoc.PseudoType

// Result of evaluation containing both console output and updated variables
case class EvalResult(console: ConsoleOutput, vars: VarMap)

object PseudoInterpreter {
  /**
   * Parse, type check, and evaluate a program from source code
   * 
   * @param source The program source code
   * @param console Console output implementation
   * @return Either an error message or the evaluation result
   */
  def parseTypeCheckAndEval(
      source: String,
      console: ConsoleOutput = DefaultConsoleOutput()
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
              (result, statement) => evalWithVars(statement, result.vars, result.console)
            }
            
            Right(result)
        }
    }
  }
  
  /**
   * Create a VarMap from variable declarations
   */
  private def createVarMapFromDeclarations(variables: Variables): VarMap = {
    val initialValues = variables.vars.map { varDecl =>
      val initialValue = varDecl.tpe match {
        case PseudoType.StringType => ""
        case PseudoType.IntType => 0
        case PseudoType.BoolType => false
      }
      
      (varDecl.name, initialValue)
    }
    
    VarMap(initialValues: _*)
  }
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
        val result = (f.start.value to f.end.value).foldLeft(EvalResult(console, vars)) { (current, i) =>
          val loopVars = current.vars.store(f.variable, i)
          f.statements.foldLeft(EvalResult(current.console, loopVars)) { (res, s) =>
            evalWithVars(s, res.vars, res.console)
          }
        }
        result

      case f @ FunctionCall("print", args) =>
        val str = args.foldLeft("") { case (res, expr) =>
          res + evalExpr(expr, vars)
        }
        EvalResult(console.print(str), vars)


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

  def evalExpr(expr: Expression, vars: VarMap): Any = {
    expr match {
      case b:BoolExpression => evalBoolExpr(b, vars)
      case i:IntExpression => evalIntExpr(i, vars)
      case s:StringExpression => evalStringExpr(s, vars)
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

    // Evaluate the expression based on its type
    val value = assign.value match {
      case expr: StringExpression => evalStringExpr(expr, vars)
      case expr: IntExpression    => evalIntExpr(expr, vars)
      case expr: BoolExpression   => evalBoolExpr(expr, vars)
      case _ => throw new RuntimeException(s"Unsupported expression type: ${assign.value.getClass}")
    }

    vars.store(assign.variable, value)
  }

}
