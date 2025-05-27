package pseudoc

import fastparse.*
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import pseudoc.PseudoCodeParser.*
import pseudoc.PseudoInterpreter.eval

class PseudoInterpreterTest extends AnyFunSuiteLike with Matchers:
  test("for loop"):
    val code =
      """Pour i <- 1 à 10 Faire
        |  Ecrire("Valeur de i: " + i + "\NL")
        |Fin Pour""".stripMargin
    val Parsed.Success(stmt, _) = parse(code, statement(_))
    val console = eval(stmt, Map.empty, TestConsoleOutput())
    
    // Assert that output contains expected text
    val expected = (1 to 10).map(i => s"Valeur de i: $i\n").mkString
    console.getOutput shouldBe expected
    
  test("if statement - true condition"):
    val code =
      """Si x = 5 Alors
        |  Ecrire("x is 5\NL")
        |Fin Si""".stripMargin
    val Parsed.Success(stmt, _) = parse(code, statement(_))
    val console = eval(stmt, Map("x" -> 5), TestConsoleOutput())
    
    console.getOutput shouldBe "x is 5\n"
    
  test("if statement - false condition"):
    val code =
      """Si x = 5 Alors
        |  Ecrire("x is 5\NL")
        |Fin Si""".stripMargin
    val Parsed.Success(stmt, _) = parse(code, statement(_))
    val console = eval(stmt, Map("x" -> 10), TestConsoleOutput())
    
    console.getOutput shouldBe ""
    
  test("if-else statement - true condition"):
    val code =
      """Si x = 5 Alors
        |  Ecrire("x is 5\NL")
        |Sinon
        |  Ecrire("x is not 5\NL")
        |Fin Si""".stripMargin
    val Parsed.Success(stmt, _) = parse(code, statement(_))
    val console = eval(stmt, Map("x" -> 5), TestConsoleOutput())
    
    console.getOutput shouldBe "x is 5\n"
    
  test("if-else statement - false condition"):
    val code =
      """Si x = 5 Alors
        |  Ecrire("x is 5\NL")
        |Sinon
        |  Ecrire("x is not 5\NL")
        |Fin Si""".stripMargin
    val Parsed.Success(stmt, _) = parse(code, statement(_))
    val console = eval(stmt, Map("x" -> 10), TestConsoleOutput())
    
    console.getOutput shouldBe "x is not 5\n"
    
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
    val console = eval(stmt, Map("x" -> 5), TestConsoleOutput())
    
    console.getOutput shouldBe "x is between 0 and 10\n"