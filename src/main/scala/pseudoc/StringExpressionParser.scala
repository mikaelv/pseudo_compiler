package pseudoc
import fastparse.*
import fastparse.JavaWhitespace.*
import pseudoc.Lexical.stringLiteral
import pseudoc.PseudoCodeParser.{expression, variableReference}
import pseudoc.ast.{FunctionCall, StringConcat, StringExpression, StringRef}

object StringExpressionParser {
  def stringRef[$: P](implicit symbols: SymbolTable): P[StringRef] =
    variableReference.collect { case s@StringRef(_) => s }


  def stringExpression[$: P](implicit symbols: SymbolTable): P[StringExpression] =
    (stringRef | stringLiteral)
      .rep(min=1, sep = "+")
      .map(StringConcat.apply).log

  def print[$: P](implicit symbols: SymbolTable): P[FunctionCall] = P(
    StringIn(
      "Ecrire",
      "ecrire",
      "écrire",
      "Write",
      "write",
      "Print",
      "print"
    ) ~ "(" ~ expression.rep(min=1, ",") ~ ")"
  ).map(concat => FunctionCall("print", concat))


}
