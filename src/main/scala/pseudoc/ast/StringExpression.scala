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

case class StringLiteral(value: String) extends StringExpression {
  // String literals are always type-correct
  override def typeCheck(symbolTable: SymbolTable): Either[String, Unit] = Right(())
}

case class StringRef(varName: String) extends StringExpression {
  override def typeCheck(symbolTable: SymbolTable): Either[String, Unit] = {
    symbolTable.checkVariableForType(varName, classOf[String]).map(_ => ())
  }
}