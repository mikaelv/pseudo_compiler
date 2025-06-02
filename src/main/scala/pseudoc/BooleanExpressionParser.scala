package pseudoc

import fastparse.JavaWhitespace.*
import fastparse.*
import pseudoc.ast.*
import Lexical.identifier

object BooleanExpressionParser {

  def or[$: P]: P[BoolExpression] = P(
    and ~ (StringIn("or", "OR", "ou", "OU", "||", "|") ~/ and).rep
  ).map(BoolOperations.createOr)

  def and[$: P]: P[BoolExpression] = P(
    factor ~ (StringIn("and", "AND", "et", "ET", "&&", "&") ~/ factor).rep
  ).map(BoolOperations.createAnd)

  def factor[$: P]: P[BoolExpression] = P(booleanExpression | parens)

  def parens[$: P]: P[BoolExpression] = P("(" ~/ or ~ ")")

  def boolTrue[$: P]: P[BoolLiteral] = P(
    StringIn("true", "TRUE", "True", "vrai", "VRAI", "Vrai").!
  ).map(_ => BoolLiteral(true))

  def boolFalse[$: P]: P[BoolLiteral] = P(
    StringIn("false", "FALSE", "False", "faux", "FAUX", "Faux").!
  ).map(_ => BoolLiteral(false))

  def booleanLiteral[$: P]: P[BoolLiteral] = P(boolTrue | boolFalse)

  // TODO should include "or" ? circular definition, look at Python grammar
  def booleanExpression[$: P]: P[BoolExpression] =
    (comparisonExpr | booleanLiteral | identifier.map(BoolRef.apply) ).log

  def booleanOperator[$: P]: P[ComparisonOperator] = P(
    StringIn("=", "==").map(_ => ComparisonOperator.Equal) |
      StringIn("!=", "<>").map(_ => ComparisonOperator.NotEqual) |
      StringIn("≤", "<=").map(_ => ComparisonOperator.LessThanEqual) |
      StringIn("≥", ">=").map(_ => ComparisonOperator.GreaterThanEqual) |
      StringIn("<").map(_ => ComparisonOperator.LessThan) |
      StringIn(">").map(_ => ComparisonOperator.GreaterThan)
  )

  def comparisonExpr[$: P]: P[BoolExpression] = P(
    IntExpressionParser.intExpression ~ booleanOperator ~ IntExpressionParser.intExpression
  ).map { case (left, op, right) => Comparison(left, op, right) }


}
