package pseudoc

/**
 * Sealed trait representing the types supported by the pseudo compiler
 */
sealed trait PseudoType {
  def name: String
}

object PseudoType {
  case object IntType extends PseudoType { val name = "int" }
  case object StringType extends PseudoType { val name = "string" }
  case object BoolType extends PseudoType { val name = "boolean" }
  
  def fromClass(clazz: Class[_]): PseudoType = clazz match {
    case c if c == classOf[Int] || c == classOf[java.lang.Integer] => IntType
    case c if c == classOf[String] => StringType
    case c if c == classOf[Boolean] || c == classOf[java.lang.Boolean] => BoolType
    case _ => throw new IllegalArgumentException(s"Unsupported type: ${clazz.getSimpleName}")
  }
  
  def toClass(pseudoType: PseudoType): Class[_] = pseudoType match {
    case IntType => classOf[Int]
    case StringType => classOf[String]
    case BoolType => classOf[Boolean]
  }
}

/**
 * Maintains type information for static type checking during parsing.
 */
case class SymbolTable(types: Map[String, PseudoType] = Map.empty) {
  /**
   * Get the PseudoType of a variable if it exists in the symbol table
   */
  def getType(variable: String): Option[PseudoType] = types.get(variable)
  
  /**
   * Get the Class of a variable if it exists in the symbol table (for compatibility)
   */
  def getClassType(variable: String): Option[Class[_]] = types.get(variable).map(PseudoType.toClass)
  
  /**
   * Add or update a variable's type in the symbol table
   */
  def addVariable(variable: String, tpe: PseudoType): SymbolTable =
    SymbolTable(types + (variable -> tpe))
    
  /**
   * Add or update a variable's type in the symbol table (Class[_] overload for compatibility)
   */
  def addVariable(variable: String, tpe: Class[_]): SymbolTable =
    addVariable(variable, PseudoType.fromClass(tpe))
    
  /**
   * Check if a variable exists with the expected type
   */
  def checkType(variable: String, expectedType: PseudoType): Boolean =
    types.get(variable).exists(_ == expectedType)
    
  /**
   * Check if a variable reference is valid for a specific type
   */
  def checkVariableForType(variable: String, expectedType: PseudoType): Either[String, PseudoType] = {
    types.get(variable) match {
      case Some(tpe) if tpe == expectedType => Right(expectedType)
      case Some(tpe) => Left(s"Type mismatch: Variable '$variable' is of type ${tpe.name}, expected ${expectedType.name}")
      case None => Left(s"Undefined variable: '$variable'")
    }
  }
  
  // Special case for integer variables - this helps with the tests
  def checkIntVariable(variable: String): Either[String, Unit] = {
    checkVariableForType(variable, PseudoType.IntType).map(_ => ())
  }
  
  // Special case for string variables - this helps with the tests  
  def checkStringVariable(variable: String): Either[String, Unit] = {
    checkVariableForType(variable, PseudoType.StringType).map(_ => ())
  }
  
  // Special case for boolean variables - this helps with the tests
  def checkBoolVariable(variable: String): Either[String, Unit] = {
    checkVariableForType(variable, PseudoType.BoolType).map(_ => ())
  }
}