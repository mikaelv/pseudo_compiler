package pseudoc.ast

import pseudoc.SymbolTable

/**
 * Base trait for all expressions with static type checking
 */
trait Expression { // TODO sealed
  /**
   * Check if this expression is type-correct
   * @return Either an error message or Unit if type checking passes
   */
  def typeCheck(symbolTable: SymbolTable): Either[String, Unit]
}

/**
 * Trait for expressions with a known return type
 */
trait TypedExpression[T] extends Expression { // TODO sealed
  /**
   * Get the class object for the expression type
   */
  def expressionType: Class[T]
}