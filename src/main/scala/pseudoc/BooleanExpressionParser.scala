package pseudoc

import fastparse.JavaWhitespace.*
import fastparse.*
import pseudoc.PseudoCodeParser.identifier
import pseudoc.ast.*

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

  def booleanExpression[$: P]: P[BoolExpression] =
    (booleanLiteral | identifier.map(BoolRef.apply))
}
