package pseudoc.ast


sealed trait BooleanExpression extends Expression[Boolean]

case class Comparison(left: Expression[Int], op: ComparisonOperator, right: Expression[Int]) extends BooleanExpression

enum ComparisonOperator:
  case Equal, NotEqual, LessThan, GreaterThan, LessThanEqual, GreaterThanEqual



