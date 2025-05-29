package pseudoc.ast

sealed trait StringExpression

case class StringConcat(values: Seq[StringExpression]) extends StringExpression
case class StringLiteral(value: String) extends StringExpression

case class StringRef(varName: String) extends StringExpression


