package pseudoc.ast

sealed trait Statement

case class ForLoop(
    variable: String,
    start: Int,
    end: Int,
    statements: Seq[Statement]
) extends Statement

case class IfStatement(
    condition: BooleanExpression,
    thenBranch: Seq[Statement],
    elseBranch: Option[Seq[Statement]] = None
) extends Statement

sealed trait Assignment extends Statement {
  def variable: String
}

case class StringAssignment(
    variable: String,
    value: StringExpression
) extends Assignment

case class IntAssignment(
    variable: String,
    value: IntExpression
) extends Assignment

case class FunctionCallString(fnName: String, args: Seq[StringExpression])
    extends Statement
