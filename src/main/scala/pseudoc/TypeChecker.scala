package pseudoc

import pseudoc.ast._

/**
 * Static type checker that verifies type correctness before execution
 */
object TypeChecker {
  /**
   * Type check a program consisting of variable declarations and statements
   * 
   * @param variables The variable declarations
   * @param statements The program statements
   * @return Either a type error message or Unit if type checking passes
   */
  def typeCheck(variables: Variables, statements: Seq[Statement]): Either[String, Unit] = {
    // Initialize symbol table from variable declarations
    val initialSymbolTable = initializeSymbolTable(variables)
    
    // Type check all statements
    statements.foldLeft[Either[String, SymbolTable]](Right(initialSymbolTable)) { 
      (result, statement) => result.flatMap(statement.typeCheck)
    }.map(_ => ())
  }
  
  /**
   * Type check a single statement with a given symbol table
   */
  def typeCheckStatement(statement: Statement, symbolTable: SymbolTable): Either[String, SymbolTable] = {
    statement.typeCheck(symbolTable)
  }
  
  /**
   * Initialize a symbol table from variable declarations
   */
  private def initializeSymbolTable(variables: Variables): SymbolTable = {
    val symbolTable = SymbolTable()
    
    // Add each variable to the symbol table with its declared type
    variables.vars.foldLeft(symbolTable) { (table, varDecl) =>
      table.addVariable(varDecl.name, varDecl.tpe)
    }
  }
  
  /**
   * Type check a full program and return the final symbol table if successful
   */
  def typeCheckAndGetSymbolTable(variables: Variables, statements: Seq[Statement]): Either[String, SymbolTable] = {
    val initialSymbolTable = initializeSymbolTable(variables)
    
    statements.foldLeft[Either[String, SymbolTable]](Right(initialSymbolTable)) { 
      (result, statement) => result.flatMap(statement.typeCheck)
    }
  }
}