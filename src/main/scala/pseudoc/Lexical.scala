package pseudoc

import fastparse.*
import NoWhitespace.*
import pseudoc.ast.BoolLiteral

object Lexical {

  def identifier[$: P]: P[String] =
    (CharIn("a-zA-Z") ~ CharIn("a-zA-Z_0-9").rep).!.filter(!keywords.contains(_))

  def ws[$: P]: P[Unit] = CharIn(" \n\t")

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



  // TODO factorize
  val keywords: Set[String] = (
    tolerantCases("si", "if") ++
    tolerantCases("alors", "then") ++
      tolerantCases("fin", "end") ++
      tolerantCases("ou", "or") ++
      tolerantCases("and", "et")
  ).toSet
}
