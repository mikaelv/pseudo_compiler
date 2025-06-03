package pseudoc

import fastparse.{CharIn, P}
import pseudoc.Lexical.{digits, identifier}
import pseudoc.ast.{ArrayAccess, ArrayRef, IntAddSub, IntExpression, IntLiteral, IntMultDiv, IntRef}
import fastparse.*
import JavaWhitespace.*
import pseudoc.PseudoCodeParser.variableReference
import pseudoc.PseudoType

object IntExpressionParser {

  def intLiteral[$: P]: P[IntLiteral] = P((StringIn("-").? ~~ digits).!).map(s => IntLiteral(s.toInt))

  def intExpr[$: P](implicit symbols: SymbolTable): P[IntExpression] =
    P(multDiv ~ (CharIn("+\\-").! ~ multDiv).rep).map(IntAddSub.create)

  def multDiv[$: P](implicit symbols: SymbolTable): P[IntExpression] =
    P(factor ~ (CharIn("*/").! ~ factor).rep).map(IntMultDiv.create)

  def factor[$: P](implicit symbols: SymbolTable): P[IntExpression] = P(intLiteral | arrayAccess | intRef | parens)

  def parens[$: P](implicit symbols: SymbolTable): P[IntExpression] = P("(" ~ intExpr ~ ")")

  def intRef[$: P](implicit symbols: SymbolTable): P[IntRef] = variableReference.collect {
    case i @ IntRef(_) => i
  }

  def arrayAccess[$: P](implicit symbols: SymbolTable): P[ArrayAccess] = P(
    (identifier ~ "[" ~ intExpr ~ "]").filter { case (name, _) => symbols.getType(name).contains(PseudoType.ArrayIntType) }
  ).map { case (arrayName, index) => ArrayAccess(ArrayRef(arrayName), index) }

}
