package pseudoc
import fastparse.*
import JavaWhitespace.*
import pseudoc.Lexical.identifier
import pseudoc.PseudoCodeParser.variableReference
import pseudoc.ast.{BoolRef, FunctionCallString, StringConcat, StringLiteral, StringRef}

object StringExpressionParser {
  def stringChars(c: Char): Boolean = c != '\"'

  def strChars[$: P]: P[Unit] = P(CharsWhile(stringChars))

  // TODO there might be a way to handle escape chars more gracefully
  def stringLiteral[$: P]: P[String] = P(
    "\"" ~/ (strChars).rep.! ~ "\""
  ).map(_.replaceAll("\\\\NL", "\n"))

  def stringRef[$: P](implicit symbols: SymbolTable): P[StringRef] =
    variableReference.collect { case s@StringRef(_) => s }


  def stringExpression[$: P](implicit symbols: SymbolTable): P[StringConcat] =
    (stringRef | stringLiteral.map(StringLiteral.apply))
      .rep(sep = "+")
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
