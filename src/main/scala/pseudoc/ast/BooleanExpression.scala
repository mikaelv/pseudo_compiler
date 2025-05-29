package pseudoc.ast


sealed trait BooleanExpression

case class Comparison(left: IntExpression, op: ComparisonOperator, right: IntExpression) extends BooleanExpression

enum ComparisonOperator:
  case Equal, NotEqual, LessThan, GreaterThan, LessThanEqual, GreaterThanEqual



