package pseudoc.ast

import scala.reflect.ClassTag

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
  def varTypeTag: ClassTag[varType]
}


case class StringAssignment(variable: String, value: StringExpression)
    extends Assignment {
  override type varType = StringExpression

  override def varTypeTag: ClassTag[StringExpression] = implicitly[ClassTag[StringExpression]]

}

case class IntAssignment(variable: String, value: IntExpression)
    extends Assignment {
  override type varType = IntExpression
  override def varTypeTag: ClassTag[IntExpression] = implicitly[ClassTag[IntExpression]]
}

case class BoolAssignment(variable: String, value: BoolExpression)
  extends Assignment {
  override type varType = BoolExpression
  override def varTypeTag: ClassTag[BoolExpression] = implicitly[ClassTag[BoolExpression]]
}

case class FunctionCallString(fnName: String, args: Seq[StringExpression])
    extends Statement
