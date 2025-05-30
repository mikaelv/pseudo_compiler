package pseudoc.ast

sealed trait Statement

case class ForLoop(
    variable: String,
    start: Int,
    end: Int,
    statements: Seq[Statement]
) extends Statement

case class IfStatement(
                        condition: BoolExpression,
                        thenBranch: Seq[Statement],
                        elseBranch: Option[Seq[Statement]] = None
) extends Statement

sealed trait Assignment extends Statement {
  def variable: String
  type varType
}


case class StringAssignment(variable: String, value: StringExpression)
    extends Assignment {
  override type varType = StringExpression
}

case class IntAssignment(variable: String, value: IntExpression)
    extends Assignment {
  override type varType = IntExpression
}

case class BoolAssignment(variable: String, value: BoolExpression)
  extends Assignment {
  override type varType = BoolExpression
}

case class FunctionCallString(fnName: String, args: Seq[StringExpression])
    extends Statement
