package pseudoc.ast

import scala.reflect.ClassTag
import pseudoc.{SymbolTable, PseudoType}

sealed trait Statement {
  /**
   * Type check this statement, updating the symbol table as needed
   * @return Either an error message or an updated symbol table
   */
  def typeCheck(symbolTable: SymbolTable): Either[String, SymbolTable]
}

case class ForLoop(
    variable: String,
    // TODO Int expressions
    start: IntLiteral,
    end: IntLiteral,
    statements: Seq[Statement]
) extends Statement {
  override def typeCheck(symbolTable: SymbolTable): Either[String, SymbolTable] = {
    // Add loop variable to symbol table as Int
    val loopSymbolTable = symbolTable.addVariable(variable, classOf[Int])
    
    // Type check each statement in the loop body
    statements.foldLeft[Either[String, SymbolTable]](Right(loopSymbolTable)) { 
      (result, stmt) => result.flatMap(stmt.typeCheck)
    }
  }
}

case class IfStatement(
                        condition: BoolExpression,
                        thenBranch: Seq[Statement],
                        elseBranch: Option[Seq[Statement]] = None
) extends Statement {
  override def typeCheck(symbolTable: SymbolTable): Either[String, SymbolTable] = {
    // Check condition
    condition.typeCheck(symbolTable) match {
      case Left(error) => Left(error)
      case Right(_) => 
        // Type check then branch
        val thenResult = thenBranch.foldLeft[Either[String, SymbolTable]](Right(symbolTable)) { 
          (result, stmt) => result.flatMap(stmt.typeCheck)
        }
        
        // Type check else branch if it exists
        elseBranch match {
          case Some(stmts) if thenResult.isRight => 
            stmts.foldLeft[Either[String, SymbolTable]](Right(symbolTable)) { 
              (result, stmt) => result.flatMap(stmt.typeCheck)
            }
          case _ => thenResult
        }
    }
  }
}

case class Assignment(variable: String, value: Expression) extends Statement {
  override def typeCheck(symbolTable: SymbolTable): Either[String, SymbolTable] = {
    // Type check the expression first
    value.typeCheck(symbolTable) match {
      case Left(error) => Left(error)
      case Right(_) =>
        // Check if this is a typed expression and get its type
        value match {
          case typedExpr: TypedExpression[_] =>
            val exprType = PseudoType.fromClass(typedExpr.expressionType)
            
            // Check if variable already exists with a compatible type
            symbolTable.getType(variable) match {
              case Some(varType) if varType != exprType =>
                Left(s"Type mismatch: Cannot assign ${exprType.name} to variable '$variable' of type ${varType.name}")
              case _ =>
                // Add or update variable in symbol table
                Right(symbolTable.addVariable(variable, exprType))
            }
          case _ =>
            Left(s"Cannot determine type of expression for assignment to variable '$variable'")
        }
    }
  }
}

case class FunctionCall(fnName: String, args: Seq[Expression])
    extends Statement {
  override def typeCheck(symbolTable: SymbolTable): Either[String, SymbolTable] = {
    // Type check all arguments
    val argResults = args.map(_.typeCheck(symbolTable))
    val errors = argResults.collect { case Left(error) => error }
    
    if (errors.isEmpty) Right(symbolTable) else Left(errors.mkString("\n"))
  }
}