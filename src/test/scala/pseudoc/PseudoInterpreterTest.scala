package pseudoc

import fastparse.*
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import pseudoc.PseudoCodeParser.*
import pseudoc.PseudoInterpreter.{eval, evalWithVars}
import pseudoc.ast.*

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
    
  test("variable assignment with type checking - compatible types") {
    // String variable
    val vars = Map("message" -> "")
    val assignment = StringAssignment("message", StringLiteral("Hello"))
    val result = evalWithVars(assignment, vars, TestConsoleOutput())
    
    result.vars("message") should be("Hello")
    result.console.getOutput should be("")
    
    // Integer variable with integer literal
    val intVars = Map("counter" -> 0)
    val intAssignment = IntAssignment("counter", IntLiteral(42))
    val intResult = evalWithVars(intAssignment, intVars, TestConsoleOutput())
    
    intResult.vars("counter") should be(42)
    intResult.console.getOutput should be("")
  }
  
  test("variable assignment with type checking - incompatible types") {
    // Runtime type check for variable types
    val vars = Map("counter" -> "")  // counter is a string, not an int
    val assignment = IntAssignment("counter", IntLiteral(42))
    
    val exception = intercept[RuntimeException] {
      evalWithVars(assignment, vars, TestConsoleOutput())
    }
    
    exception.getMessage should include("Type error")
    exception.getMessage should include("integer")
    exception.getMessage should include("counter")
  }
  
  test("variable assignment updates variable state for subsequent statements") {
    val vars = Map("x" -> 0)
    val statements = Seq(
      IntAssignment("x", IntLiteral(5)),
      IfStatement(
        Comparison(IntRef("x"), ComparisonOperator.Equal, IntLiteral(5)),
        Seq(FunctionCallString("print", Seq(StringConcat(Seq(StringLiteral("x is 5")))))),
        None
      )
    )
    
    val console = TestConsoleOutput()
    val result = statements.foldLeft(EvalResult(console, vars)) { (res, stmt) =>
      evalWithVars(stmt, res.vars, res.console)
    }
    
    result.console.getOutput should be("x is 5")
    result.vars("x") should be(5)
  }
  
  test("integer assignment with parsed code") {
    // Test with just the assignment portion
    val assignmentCode = "x <- 42"
    val Parsed.Success(assignmentStmt, _) = parse(assignmentCode, intAssignment(_))
    
    val vars = Map("x" -> 0)
    val console = TestConsoleOutput()
    
    val result = evalWithVars(assignmentStmt, vars, console)
    
    result.vars("x") should be(42)
  }
  
  test("string assignment with parsed code") {
    // Test with just the assignment portion
    val assignmentCode = "message <- \"Hello\""
    val Parsed.Success(assignmentStmt, _) = parse(assignmentCode, stringAssignment(_))
    
    val vars = Map("message" -> "")
    val console = TestConsoleOutput()
    
    val result = evalWithVars(assignmentStmt, vars, console)
    
    result.vars("message") should be("Hello")
  }
  
  test("sequence of statements with assignment") {
    // First parse just the assignment
    val assignmentCode = "x <- 42"
    val Parsed.Success(assignStmt, _) = parse(assignmentCode, intAssignment(_))
    
    // Then parse an if statement
    val ifCode = 
      """Si x = 42 Alors
        |  Ecrire("x is 42\NL")
        |Fin Si""".stripMargin
    val Parsed.Success(ifStmt, _) = parse(ifCode, ifStatement(_))
    
    // Execute them in sequence
    val vars = Map("x" -> 0)
    val console = TestConsoleOutput()
    
    val result1 = evalWithVars(assignStmt, vars, console)
    val result2 = evalWithVars(ifStmt, result1.vars, result1.console)
    
    result2.console.getOutput should be("x is 42\n")
    result2.vars("x") should be(42)
  }

  test("arithmetic operation") {
    val code = "x <- 2 + 3*(x+3) - y"
    val Parsed.Success(stmt, _) = parse(code, assignment(_))
    val result = evalWithVars(stmt, Map("x" -> 2, "y" -> 4)).vars
    result should be(Map("x" -> 13, "y" -> 4))
  }