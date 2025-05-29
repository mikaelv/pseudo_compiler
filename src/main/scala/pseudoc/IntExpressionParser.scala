package pseudoc

import fastparse.{CharIn, P}
import pseudoc.PseudoCodeParser.{identifier, integer}
import pseudoc.ast.{IntAddSub, IntExpression, IntLiteral, IntMultDiv, IntRef}
import fastparse.*
import JavaWhitespace.*


object IntExpressionParser {

  def addSub[$: P]: P[IntAddSub] = P(multDiv ~ (CharIn("+\\-").! ~/ multDiv).rep).map(IntAddSub.create)

  def multDiv[$: P]: P[IntMultDiv] = P(factor ~ (CharIn("*/").! ~/ factor).rep).map(IntMultDiv.create)

  def factor[$: P]: P[IntExpression] = P(intExpression | parens)

  def parens[$: P]: P[IntAddSub] = P("(" ~/ addSub ~ ")")

  def intExpression[$: P]: P[IntExpression] =
    (integer.map(IntLiteral.apply) | identifier.map(IntRef.apply))
}
