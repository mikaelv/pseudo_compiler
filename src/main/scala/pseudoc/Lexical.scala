package pseudoc

import fastparse._
import NoWhitespace._

object Lexical {

  def identifier[$: P]: P[String] =
    (CharIn("a-zA-Z") ~ CharIn("a-zA-Z_0-9").rep).!.filter(!keywords.contains(_))

  def ws[$: P]: P[Unit] = CharIn(" \n\t")

  def tolerantCases(s: String): Seq[String] =
    Seq(s, s.head.toUpper + s.tail, s.toUpperCase)

  def tolerantCases(fr: String, en: String): Seq[String] =
    tolerantCases(fr) ++ tolerantCases(en)


  // TODO factorize
  val keywords: Set[String] = (
    tolerantCases("si", "if") ++
    tolerantCases("alors", "then") ++
      tolerantCases("fin", "end") ++
      tolerantCases("ou", "or") ++
      tolerantCases("and", "et")
  ).toSet
}
