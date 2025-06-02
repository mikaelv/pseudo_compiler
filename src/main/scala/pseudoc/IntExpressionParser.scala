package pseudoc

import fastparse.{CharIn, P}
import pseudoc.Lexical.identifier
import pseudoc.ast.{IntAddSub, IntExpression, IntLiteral, IntMultDiv, IntRef}
import fastparse.*
import JavaWhitespace.*
import pseudoc.PseudoCodeParser.variableReference

object IntExpressionParser {

  def digits[$: P]: P[Unit] = P(CharsWhileIn("0-9"))
  def intLiteral[$: P]: P[IntLiteral] = digits.!.map(s => IntLiteral(s.toInt))

  def intExpr[$: P](implicit symbols: SymbolTable): P[IntExpression] =
    P(multDiv ~ (CharIn("+\\-").! ~ multDiv).rep).map(IntAddSub.create)

  def multDiv[$: P](implicit symbols: SymbolTable): P[IntExpression] =
    P(factor ~ (CharIn("*/").! ~ factor).rep).map(IntMultDiv.create)

  def factor[$: P](implicit symbols: SymbolTable): P[IntExpression] = P(intFactor | parens)

  def parens[$: P](implicit symbols: SymbolTable): P[IntExpression] = P("(" ~ intExpr ~ ")")

  def intRef[$: P](implicit symbols: SymbolTable): P[IntRef] = variableReference.collect {
    case i @ IntRef(_) => i
  }

  def intFactor[$: P](implicit symbols: SymbolTable): P[IntExpression] =
    (intLiteral | intRef)

}
