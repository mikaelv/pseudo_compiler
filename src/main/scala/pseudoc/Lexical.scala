package pseudoc

import fastparse._
import NoWhitespace._

object Lexical {

  def identifier[$: P]: P[String] =
    (CharIn("a-zA-Z") ~ CharIn("a-zA-Z_0-9").rep).!.filter(!keywords.contains(_))


  // TODO factorize
  val keywords: Set[String] = Set(
    "Si", "si", "If", "if",
    "Alors", "alors", "Then", "then",
    "Sinon", "sinon", "Else", "else",
    "Fin", "fin", "End", "end"
  )
}
