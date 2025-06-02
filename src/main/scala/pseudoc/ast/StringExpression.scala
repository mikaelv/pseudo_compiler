package pseudoc.ast

import pseudoc.SymbolTable

sealed trait StringExpression extends TypedExpression[String] {
  override def expressionType: Class[String] = classOf[String]
}

case class StringConcat(values: Seq[StringExpression]) extends StringExpression {
  override def typeCheck(symbolTable: SymbolTable): Either[String, Unit] = {
    // Check each component of the concatenation
    val results = values.map(_.typeCheck(symbolTable))
    val errors = results.collect { case Left(error) => error }
    
    if (errors.isEmpty) Right(()) else Left(errors.mkString("\n"))
  }
}

object StringConcat {
  def apply(values: Seq[StringExpression]): StringExpression = {
    require(values.nonEmpty)
    if (values.length == 1)
      values.head
    else
      new StringConcat(values)
  }
}

case class StringLiteral(value: String) extends StringExpression {
  // String literals are always type-correct
  override def typeCheck(symbolTable: SymbolTable): Either[String, Unit] = Right(())
}

case class StringRef(varName: String) extends StringExpression {
  override def typeCheck(symbolTable: SymbolTable): Either[String, Unit] = {
    // Strict type checking - only allow reference to string variables
    symbolTable.checkStringVariable(varName)
  }
}