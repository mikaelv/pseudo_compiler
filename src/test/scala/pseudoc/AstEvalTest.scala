package pseudoc

import org.scalatest.funsuite.AnyFunSuiteLike
import fastparse.*
import Ast.*
import PseudoCodeParser.*
import fastparse.Parsed.Success

class AstEvalTest extends AnyFunSuiteLike:
  test("for loop"):
    val code =
      """Pour i <- 1 à 10 Faire
        |  Ecrire("Valeur de i: " + i + "\NL")
        |Fin Pour""".stripMargin
    val Parsed.Success(stmt, _) = parse(code, statement(_))
    Ast.eval(stmt, Map.empty)
