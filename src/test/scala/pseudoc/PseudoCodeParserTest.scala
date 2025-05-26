package pseudoc

import org.scalatest.funsuite.AnyFunSuiteLike
import PseudoCodeParser.*
import fastparse.*
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers.*
import pseudoc.Ast.{Algorithm, VariableDecl, Variables}


class PseudoCodeParserTest extends AnyFunSuiteLike:
  def check[A](str: String, myParser: P[_] => P[A], expectedValue: A): Assertion =
    val result = parse(str, myParser(_))
    result.get.value should === (expectedValue)

  test("algorithme"):
    check("Algorithme: recherche_dichotomique1", algo(_), Algorithm("recherche_dichotomique1"))
    check("Algorithme  :algo2", algo(_), Algorithm("algo2"))


  test("string variable"):
    check("chaine1: chaîne", variableDecl(_), VariableDecl("chaine1", "string"))


  test("variables"):
    check("Variables:\nchaine1: string", variables(_), Variables(Seq(VariableDecl("chaine1", "string"))))
    var result = parse("Variables:\n chaine1: chaîne, chaine2: chaîne de caractères, chaine3: string", variables(_))
    result.get.value should === (Variables(Seq(
      VariableDecl("chaine1", "string"),
      VariableDecl("chaine2", "string"),
      VariableDecl("chaine3", "string"),
    )))


