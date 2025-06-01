package pseudoc

/**
 * Maintains type information for static type checking during parsing.
 */
case class SymbolTable(types: Map[String, Class[_]] = Map.empty) {
  /**
   * Get the type of a variable if it exists in the symbol table
   */
  def getType(variable: String): Option[Class[_]] = types.get(variable)
  
  /**
   * Add or update a variable's type in the symbol table
   */
  def addVariable(variable: String, tpe: Class[_]): SymbolTable =
    SymbolTable(types + (variable -> tpe))
    
  /**
   * Check if a variable exists with the expected type
   */
  def checkType(variable: String, expectedType: Class[_]): Boolean =
    types.get(variable).exists(_ == expectedType)
    
  /**
   * Check if a variable reference is valid for a specific type
   */
  def checkVariableForType[T](variable: String, expectedType: Class[T]): Either[String, Class[T]] = {
    types.get(variable) match {
      case Some(tpe) if tpe == expectedType => Right(expectedType)
      case Some(tpe) => Left(s"Type mismatch: Variable '$variable' is of type ${tpe.getSimpleName}, expected ${expectedType.getSimpleName}")
      case None => Left(s"Undefined variable: '$variable'")
    }
  }
}