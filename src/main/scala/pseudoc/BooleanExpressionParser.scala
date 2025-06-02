package pseudoc

import fastparse.JavaWhitespace.*
import fastparse.*
import pseudoc.ast.*
import Lexical.{booleanLiteral, identifier}
import pseudoc.PseudoCodeParser.variableReference

object BooleanExpressionParser {

  def or[$: P](implicit symbols: SymbolTable): P[BoolExpression] = P(
    and ~ (StringIn("or", "OR", "ou", "OU", "||", "|") ~/ and).rep
  ).map(BoolOperations.createOr)

  def and[$: P](implicit symbols: SymbolTable): P[BoolExpression] = P(
    boolFactor ~ (StringIn("and", "AND", "et", "ET", "&&", "&") ~/ boolFactor).rep
  ).map(BoolOperations.createAnd)


  def parens[$: P](implicit symbols: SymbolTable): P[BoolExpression] = P("(" ~/ or ~ ")")

  def boolFactor[$: P](implicit symbols: SymbolTable): P[BoolExpression] =
    // TODO testcase for _.asInstanceOf
    (booleanLiteral | comparisonExpr | variableReference.map(_.asInstanceOf[BoolRef]) | parens).log

  def booleanOperator[$: P]: P[ComparisonOperator] = P(
    StringIn("=", "==").map(_ => ComparisonOperator.Equal) |
      StringIn("!=", "<>").map(_ => ComparisonOperator.NotEqual) |
      StringIn("≤", "<=").map(_ => ComparisonOperator.LessThanEqual) |
      StringIn("≥", ">=").map(_ => ComparisonOperator.GreaterThanEqual) |
      StringIn("<").map(_ => ComparisonOperator.LessThan) |
      StringIn(">").map(_ => ComparisonOperator.GreaterThan)
  )

  def comparisonExpr[$: P](implicit symbols: SymbolTable): P[BoolExpression] = P(
    IntExpressionParser.intFactor ~ booleanOperator ~ IntExpressionParser.intFactor
  ).map { case (left, op, right) => Comparison(left, op, right) }


}
