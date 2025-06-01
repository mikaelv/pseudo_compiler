package pseudoc

import fastparse._
import NoWhitespace._

object Lexical {

  def identifier[$: P]: P[String] =
    (CharIn("a-zA-Z") ~ CharIn("a-zA-Z_0-9").rep).!

}
