package pseudoc
import fastparse.*
import fastparse.JavaWhitespace.*
import pseudoc.Lexical.stringLiteral
import pseudoc.PseudoCodeParser.variableReference
import pseudoc.ast.{FunctionCallString, StringConcat, StringRef}

object StringExpressionParser {
  def stringRef[$: P](implicit symbols: SymbolTable): P[StringRef] =
    variableReference.collect { case s@StringRef(_) => s }


  def stringExpression[$: P](implicit symbols: SymbolTable): P[StringConcat] =
    (stringRef | stringLiteral)
      .rep(min=1, sep = "+")
      .map(StringConcat.apply).log

  def print[$: P](implicit symbols: SymbolTable): P[FunctionCallString] = P(
    StringIn(
      "Ecrire",
      "ecrire",
      "écrire",
      "Write",
      "write",
      "Print",
      "print"
    ) ~ "(" ~ stringExpression ~ ")"
  ).map(concat => FunctionCallString("print", Seq(concat)))


}
