package pseudoc.ast

import pseudoc.SymbolTable

sealed trait ArrayExpression extends TypedExpression[Array[Int]] {
  override def expressionType: Class[Array[Int]] = classOf[Array[Int]]
}

case class ArrayRef(varName: String) extends ArrayExpression {
  override def typeCheck(symbolTable: SymbolTable): Either[String, Unit] = {
    symbolTable.checkArrayIntVariable(varName)
  }
}

case class ArrayLiteral(values: Seq[IntExpression]) extends ArrayExpression {
  override def typeCheck(symbolTable: SymbolTable): Either[String, Unit] = {
    val errors = values.map(_.typeCheck(symbolTable)).collect { case Left(error) => error }
    if (errors.isEmpty) Right(()) else Left(errors.mkString("\n"))
  }
}

