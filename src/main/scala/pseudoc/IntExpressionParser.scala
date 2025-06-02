package pseudoc

import fastparse.{CharIn, P}
import pseudoc.Lexical.identifier
import pseudoc.ast.{IntAddSub, IntExpression, IntLiteral, IntMultDiv, IntRef}
import fastparse.*
import JavaWhitespace.*
import pseudoc.PseudoCodeParser.variableReference


object IntExpressionParser {

  def digits[$: P]: P[Unit] = P(CharsWhileIn("0-9"))
  def integer[$: P]: P[IntLiteral] = digits.!.map(s => IntLiteral(s.toInt))

  def addSub[$: P](implicit symbols: SymbolTable): P[IntExpression] = P(multDiv ~ (CharIn("+\\-").! ~ multDiv).rep).map(IntAddSub.create)

  def multDiv[$: P](implicit symbols: SymbolTable): P[IntExpression] = P(factor ~ (CharIn("*/").! ~ factor).rep).map(IntMultDiv.create)

  def factor[$: P](implicit symbols: SymbolTable): P[IntExpression] = P(intExpression | parens)

  def parens[$: P](implicit symbols: SymbolTable): P[IntExpression] = P("(" ~ addSub ~ ")")

  def intExpression[$: P](implicit symbols: SymbolTable): P[IntExpression] =
    (integer | variableReference.map(_.asInstanceOf[IntRef]))
}
