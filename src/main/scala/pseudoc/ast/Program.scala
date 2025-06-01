package pseudoc.ast

import pseudoc.SymbolTable

/**
 * Represents a complete program with algorithm name, variable declarations, and statements
 */
case class Program(
  algorithm: Algorithm,
  variables: Variables,
  statements: Seq[Statement]
) {
  /**
   * Type check this program
   * @return Either an error message or the final symbol table
   */
  def typeCheck(): Either[String, SymbolTable] = {
    // Initialize symbol table from variable declarations
    val initialSymbolTable = initializeSymbolTable()
    
    // Type check all statements
    statements.foldLeft[Either[String, SymbolTable]](Right(initialSymbolTable)) { 
      (result, statement) => result.flatMap(statement.typeCheck)
    }
  }
  
  /**
   * Initialize a symbol table from this program's variable declarations
   */
  private def initializeSymbolTable(): SymbolTable = {
    val symbolTable = SymbolTable()
    
    // Add each variable to the symbol table with its declared type
    variables.vars.foldLeft(symbolTable) { (table, varDecl) =>
      val tpe = varDecl.tpe match {
        case "string" => classOf[String]
        case "int" | "integer" => classOf[Int]
        case "bool" | "boolean" => classOf[Boolean]
        case other => throw new IllegalArgumentException(s"Unsupported type: $other")
      }
      
      table.addVariable(varDecl.name, tpe)
    }
  }
}