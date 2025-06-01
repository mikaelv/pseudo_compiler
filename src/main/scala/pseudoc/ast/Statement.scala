package pseudoc.ast

import scala.reflect.ClassTag
import pseudoc.SymbolTable

sealed trait Statement {
  /**
   * Type check this statement, updating the symbol table as needed
   * @return Either an error message or an updated symbol table
   */
  def typeCheck(symbolTable: SymbolTable): Either[String, SymbolTable]
}

case class ForLoop(
    variable: String,
    start: Int,
    end: Int,
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

sealed trait Assignment extends Statement {
  def variable: String
  type varType
  def varTypeTag: ClassTag[varType]
}

case class StringAssignment(variable: String, value: StringExpression)
    extends Assignment {
  override type varType = StringExpression
  override def varTypeTag: ClassTag[StringExpression] = implicitly[ClassTag[StringExpression]]
  
  override def typeCheck(symbolTable: SymbolTable): Either[String, SymbolTable] = {
    // Type check the expression
    value.typeCheck(symbolTable).map { _ =>
      // Check if variable already exists with a different type
      if (symbolTable.getType(variable).exists(_ != classOf[String])) {
        Left(s"Type mismatch: Cannot assign String to variable '$variable' of type ${symbolTable.getType(variable).get.getSimpleName}")
      } else {
        // Add or update variable in symbol table
        Right(symbolTable.addVariable(variable, classOf[String]))
      }
    }.flatten
  }
}

case class IntAssignment(variable: String, value: IntExpression)
    extends Assignment {
  override type varType = IntExpression
  override def varTypeTag: ClassTag[IntExpression] = implicitly[ClassTag[IntExpression]]
  
  override def typeCheck(symbolTable: SymbolTable): Either[String, SymbolTable] = {
    // Type check the expression
    value.typeCheck(symbolTable).map { _ =>
      // Check if variable already exists with a different type
      if (symbolTable.getType(variable).exists(_ != classOf[Int])) {
        Left(s"Type mismatch: Cannot assign Int to variable '$variable' of type ${symbolTable.getType(variable).get.getSimpleName}")
      } else {
        // Add or update variable in symbol table
        Right(symbolTable.addVariable(variable, classOf[Int]))
      }
    }.flatten
  }
}

case class BoolAssignment(variable: String, value: BoolExpression)
  extends Assignment {
  override type varType = BoolExpression
  override def varTypeTag: ClassTag[BoolExpression] = implicitly[ClassTag[BoolExpression]]
  
  override def typeCheck(symbolTable: SymbolTable): Either[String, SymbolTable] = {
    // Type check the expression
    value.typeCheck(symbolTable).map { _ =>
      // Check if variable already exists with a different type
      if (symbolTable.getType(variable).exists(_ != classOf[Boolean])) {
        Left(s"Type mismatch: Cannot assign Boolean to variable '$variable' of type ${symbolTable.getType(variable).get.getSimpleName}")
      } else {
        // Add or update variable in symbol table
        Right(symbolTable.addVariable(variable, classOf[Boolean]))
      }
    }.flatten
  }
}

case class FunctionCallString(fnName: String, args: Seq[StringExpression])
    extends Statement {
  override def typeCheck(symbolTable: SymbolTable): Either[String, SymbolTable] = {
    // Type check all arguments
    val argResults = args.map(_.typeCheck(symbolTable))
    val errors = argResults.collect { case Left(error) => error }
    
    if (errors.isEmpty) Right(symbolTable) else Left(errors.mkString("\n"))
  }
}