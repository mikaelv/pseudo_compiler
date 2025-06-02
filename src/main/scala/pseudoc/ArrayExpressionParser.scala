package pseudoc

import fastparse.*
import fastparse.JavaWhitespace.*
import pseudoc.IntExpressionParser.intExpr
import pseudoc.PseudoCodeParser.variableReference
import pseudoc.ast.*

object ArrayExpressionParser {

  def arrayRef[$: P](implicit symbols: SymbolTable): P[ArrayRef] = P(
    variableReference.collect { case a @ ArrayRef(_) => a })

  def arrayLiteral[$: P](implicit symbols: SymbolTable): P[ArrayLiteral] = P(
    "{" ~ intExpr.rep(sep = ",") ~ "}"
  ).map(ArrayLiteral.apply)

  def arrayExpr[$: P](implicit symbols: SymbolTable): P[ArrayExpression] = P(
    arrayLiteral | arrayRef
  )
}