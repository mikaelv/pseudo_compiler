package pseudoc.ast

sealed trait BoolExpression

case class Comparison(
    left: IntExpression,
    op: ComparisonOperator,
    right: IntExpression
) extends BoolExpression

enum ComparisonOperator:
  case Equal, NotEqual, LessThan, GreaterThan, LessThanEqual, GreaterThanEqual

case class BoolRef(varName: String) extends BoolExpression

case class BoolLiteral(value: Boolean) extends BoolExpression

enum BooleanOperator:
  case And, Or

case class BoolOperations(base: BoolExpression, ops: (BooleanOperator, BoolExpression)*)
    extends BoolExpression

object BoolOperations {
  def createAnd(head: BoolExpression, tail: Seq[BoolExpression]): BoolExpression =
    if (tail.isEmpty) head
    else
      new BoolOperations(head, tail.map { expr => (BooleanOperator.And, expr) }: _*)

  def createOr(head: BoolExpression, tail: Seq[BoolExpression]): BoolExpression =
    if (tail.isEmpty) head
    else
      new BoolOperations(head, tail.map { expr => (BooleanOperator.Or, expr) }: _*)
}
