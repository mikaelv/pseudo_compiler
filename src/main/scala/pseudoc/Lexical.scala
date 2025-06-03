package pseudoc

import fastparse.*
import NoWhitespace.*
import pseudoc.ast.{BoolLiteral, StringLiteral}

object Lexical {

  def identifier[$: P]: P[String] =
    (CharIn("a-zA-Z") ~ CharIn("a-zA-Z_0-9").rep).!.filter(!keywords.contains(_))

  def space[$: P]: P[Unit] = P(CharIn(" \t"))
  def spaceLF[$: P]: P[Unit] = P(CharIn(" \r\n\t"))

  def lineFeed[$: P]: P[Unit] = P(space.rep ~ CharIn("\n", "\r").rep(1) ~ spaceLF.rep)

  def tolerantCases(s: String): Seq[String] =
    Seq(s, s.head.toUpper + s.tail, s.toUpperCase)

  def tolerantCases(fr: String, en: String): Seq[String] =
    tolerantCases(fr) ++ tolerantCases(en)

  def boolTrue[$: P]: P[BoolLiteral] = P(
    StringIn("true", "TRUE", "True", "vrai", "VRAI", "Vrai").!
  ).map(_ => BoolLiteral(true))

  def boolFalse[$: P]: P[BoolLiteral] = P(
    StringIn("false", "FALSE", "False", "faux", "FAUX", "Faux").!
  ).map(_ => BoolLiteral(false))

  def booleanLiteral[$: P]: P[BoolLiteral] = P(boolTrue | boolFalse)

  def stringChars(c: Char): Boolean = c != '\"'

  def strChars[$: P]: P[Unit] = P(CharsWhile(stringChars))

  // TODO there might be a way to handle escape chars more gracefully
  def stringLiteral[$: P]: P[StringLiteral] = P(
    "\"" ~/ (strChars).rep.! ~ "\""
  ).map(_.replaceAll("\\\\NL", "\n")).map(StringLiteral.apply)

  def stringType[$: P]: P[PseudoType.StringType.type] = StringIn(
    "chaine de caracteres",
    "chaine de caractères",
    "chaîne de caractères",
    "chaîne",
    "chaine",
    "string",
    "str"
  ).map(_ => PseudoType.StringType)

  def intType[$: P]: P[PseudoType.IntType.type] =
    StringIn("int", "integer", "entier").map(_ => PseudoType.IntType)

  def boolType[$: P]: P[PseudoType.BoolType.type] =
    StringIn("bool", "boolean", "booléen").map(_ => PseudoType.BoolType)

  def arrayIntType[$: P]: P[PseudoType.ArrayIntType.type] =
    StringIn(
      "tableau[entier]", 
      "array[int]", 
      "arrayint",
      "tableau d'entier",
      "tableau d'entiers", 
      "array of integer",
      "array of integers"
    ).map(_ => PseudoType.ArrayIntType)

  def tpe[$: P]: P[PseudoType] = P(arrayIntType | stringType | intType | boolType)

  // TODO factorize
  val keywords: Set[String] = (
    tolerantCases("si", "if") ++
      tolerantCases("alors", "then") ++
      tolerantCases("fin", "end") ++
      tolerantCases("ou", "or") ++
      tolerantCases("et", "and") ++
      tolerantCases("vrai", "true") ++
      tolerantCases("faux", "false")
  ).toSet
}
