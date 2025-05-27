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
    
  test("if statement - true condition"):
    val code =
      """Si x = 5 Alors
        |  Ecrire("x is 5\NL")
        |Fin Si""".stripMargin
    val Parsed.Success(stmt, _) = parse(code, statement(_))
    Ast.eval(stmt, Map("x" -> 5))
    
  test("if statement - false condition"):
    val code =
      """Si x = 5 Alors
        |  Ecrire("x is 5\NL")
        |Fin Si""".stripMargin
    val Parsed.Success(stmt, _) = parse(code, statement(_))
    Ast.eval(stmt, Map("x" -> 10))
    
  test("if-else statement - true condition"):
    val code =
      """Si x = 5 Alors
        |  Ecrire("x is 5\NL")
        |Sinon
        |  Ecrire("x is not 5\NL")
        |Fin Si""".stripMargin
    val Parsed.Success(stmt, _) = parse(code, statement(_))
    Ast.eval(stmt, Map("x" -> 5))
    
  test("if-else statement - false condition"):
    val code =
      """Si x = 5 Alors
        |  Ecrire("x is 5\NL")
        |Sinon
        |  Ecrire("x is not 5\NL")
        |Fin Si""".stripMargin
    val Parsed.Success(stmt, _) = parse(code, statement(_))
    Ast.eval(stmt, Map("x" -> 10))
    
  test("nested if statements"):
    val code =
      """Si x > 0 Alors
        |  Si x < 10 Alors
        |    Ecrire("x is between 0 and 10\NL")
        |  Sinon
        |    Ecrire("x is greater than or equal to 10\NL")
        |  Fin Si
        |Sinon
        |  Ecrire("x is less than or equal to 0\NL")
        |Fin Si""".stripMargin
    val Parsed.Success(stmt, _) = parse(code, statement(_))
    Ast.eval(stmt, Map("x" -> 5))