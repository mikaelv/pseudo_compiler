package pseudoc

import fastparse.*
import fastparse.JavaWhitespace.*
import pseudoc.Lexical.booleanLiteral
import pseudoc.PseudoCodeParser.variableReference
import pseudoc.ast.*

object BooleanExpressionParser {

  def boolExpr[$: P](implicit symbols: SymbolTable): P[BoolExpression] = P(
    and ~ (StringIn("or", "OR", "ou", "OU", "||", "|") ~/ and).rep
  ).map(BoolOperations.createOr)

  def and[$: P](implicit symbols: SymbolTable): P[BoolExpression] = P(
    boolFactor ~ (StringIn("and", "AND", "et", "ET", "&&", "&") ~/ boolFactor).rep
  ).map(BoolOperations.createAnd)


  def parens[$: P](implicit symbols: SymbolTable): P[BoolExpression] = P("(" ~/ boolExpr ~ ")")

  def boolRef[$: P](implicit symbols: SymbolTable): P[BoolRef] =
    variableReference.collect { case b@BoolRef(_) => b }

  def boolFactor[$: P](implicit symbols: SymbolTable): P[BoolExpression] = P(
    (booleanLiteral | comparisonExpr | boolRef | parens)
  )

  def booleanOperator[$: P]: P[ComparisonOperator] = P(
    StringIn("=", "==").map(_ => ComparisonOperator.Equal) |
      StringIn("!=", "<>").map(_ => ComparisonOperator.NotEqual) |
      StringIn("≤", "<=").map(_ => ComparisonOperator.LessThanEqual) |
      StringIn("≥", ">=").map(_ => ComparisonOperator.GreaterThanEqual) |
      StringIn("<").map(_ => ComparisonOperator.LessThan) |
      StringIn(">").map(_ => ComparisonOperator.GreaterThan)
  )

  def comparisonExpr[$: P](implicit symbols: SymbolTable): P[BoolExpression] = P(
    IntExpressionParser.factor ~ booleanOperator ~ IntExpressionParser.factor
  ).map { case (left, op, right) => Comparison(left, op, right) }


}
