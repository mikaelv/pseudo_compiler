package pseudoc

import fastparse.{CharIn, P}
import pseudoc.Lexical.identifier
import pseudoc.ast.{IntAddSub, IntExpression, IntLiteral, IntMultDiv, IntRef}
import fastparse.*
import JavaWhitespace.*


object IntExpressionParser {

  def digits[$: P]: P[Unit] = P(CharsWhileIn("0-9"))
  def integer[$: P]: P[Int] = digits.!.map(_.toInt)

  def addSub[$: P]: P[IntExpression] = P(multDiv ~ (CharIn("+\\-").! ~/ multDiv).rep).map(IntAddSub.create)

  def multDiv[$: P]: P[IntExpression] = P(factor ~ (CharIn("*/").! ~/ factor).rep).map(IntMultDiv.create)

  def factor[$: P]: P[IntExpression] = P(intExpression | parens)

  def parens[$: P]: P[IntExpression] = P("(" ~/ addSub ~ ")")

  def intExpression[$: P]: P[IntExpression] =
    (integer.map(IntLiteral.apply) | identifier.map(IntRef.apply))
}
