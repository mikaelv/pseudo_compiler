package pseudoc

import fastparse.*
import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import pseudoc.PseudoCodeParser.*
import pseudoc.PseudoInterpreter.{eval, evalStmt}
import pseudoc.PseudoType.{ArrayIntType, BoolType, IntType}
import pseudoc.ast.*

class PseudoInterpreterTest extends AnyFunSuiteLike with Matchers with EitherValues:
  def statementNoSymbols[$: P]: P[Statement] = PseudoCodeParser.statement(symbols = SymbolTable())

  def assignmentNoSymbols[$: P]: P[Assignment] = PseudoCodeParser.assignment(symbols = SymbolTable())

  test("for loop"):
    implicit val symbols: SymbolTable = SymbolTable(Map("i" -> IntType))
    val code =
      """Pour i <- 1 à 10 Faire
        |  Ecrire("Valeur de i: ", i, "\NL")
        |Fin Pour""".stripMargin
    val Parsed.Success(stmt, _) = parse(code, statement(_))
    val console = eval(stmt, VarMap.empty, TestConsoleIO())

    // Assert that output contains expected text
    val expected = (1 to 10).map(i => s"Valeur de i: $i\n").mkString
    console.getOutput shouldBe expected

  test("if statement - true condition"):
    implicit val symbols: SymbolTable = SymbolTable(Map("x" -> IntType))
    val code =
      """Si x = 5 Alors
        |  Ecrire("x is 5\NL")
        |Fin Si""".stripMargin
    val Parsed.Success(stmt, _) = parse(code, statement(_))
    val console = eval(stmt, VarMap("x" -> 5), TestConsoleIO())

    console.getOutput shouldBe "x is 5\n"

  test("if statement - false condition"):
    implicit val symbols: SymbolTable = SymbolTable(Map("x" -> IntType))
    val code =
      """Si x = 5 Alors
        |  Ecrire("x is 5\NL")
        |Fin Si""".stripMargin
    val Parsed.Success(stmt, _) = parse(code, statement(_))
    val console = eval(stmt, VarMap("x" -> 10), TestConsoleIO())

    console.getOutput shouldBe ""

  test("if-else statement - true condition"):
    implicit val symbols: SymbolTable = SymbolTable(Map("x" -> IntType))
    val code =
      """Si x = 5 Alors
        |  Ecrire("x is 5\NL")
        |Sinon
        |  Ecrire("x is not 5\NL")
        |Fin Si""".stripMargin
    val Parsed.Success(stmt, _) = parse(code, statement(_))
    val console = eval(stmt, VarMap("x" -> 5), TestConsoleIO())

    console.getOutput shouldBe "x is 5\n"

  test("if-else statement - false condition"):
    implicit val symbols: SymbolTable = SymbolTable(Map("x" -> IntType))
    val code =
      """Si x = 5 Alors
        |  Ecrire("x is 5\NL")
        |Sinon
        |  Ecrire("x is not 5\NL")
        |Fin Si""".stripMargin
    val Parsed.Success(stmt, _) = parse(code, statement(_))
    val console = eval(stmt, VarMap("x" -> 10), TestConsoleIO())

    console.getOutput shouldBe "x is not 5\n"

  test("nested if statements"):
    implicit val symbols: SymbolTable = SymbolTable(Map("x" -> IntType))
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
    val console = eval(stmt, VarMap("x" -> 5), TestConsoleIO())

    console.getOutput shouldBe "x is between 0 and 10\n"

  test("variable assignment with type checking - compatible types") {
    // String variable
    val vars = VarMap("message" -> "")
    val assignment = Assignment("message", StringLiteral("Hello"))
    val result = evalStmt(assignment, vars, TestConsoleIO())

    result.vars("message") should be("Hello")
    result.console.getOutput should be("")

    // Integer variable with integer literal
    val intVars = VarMap("counter" -> 0)
    val intAssignment = Assignment("counter", IntLiteral(42))
    val intResult = evalStmt(intAssignment, intVars, TestConsoleIO())

    intResult.vars("counter") should be(42)
    intResult.console.getOutput should be("")
  }

  test("variable assignment with type checking - incompatible types") {
    // Runtime type check for variable types
    val vars = VarMap("counter" -> "") // counter is a string, not an int
    val assignment = Assignment("counter", IntLiteral(42))

    val exception = intercept[RuntimeException] {
      evalStmt(assignment, vars, TestConsoleIO())
    }

    exception.getMessage should include("Type error")
    exception.getMessage should include("Integer")
    exception.getMessage should include("counter")
  }

  test("variable assignment updates variable state for subsequent statements") {
    val vars = VarMap("x" -> 0)
    val statements = Seq(
      Assignment("x", IntLiteral(5)),
      IfStatement(
        Comparison(IntRef("x"), ComparisonOperator.Equal, IntLiteral(5)),
        Seq(FunctionCall("print", Seq(StringConcat(Seq(StringLiteral("x is 5")))))),
        None
      )
    )

    val console = TestConsoleIO()
    val result = statements.foldLeft(EvalResult(console, vars)) { (res, stmt) =>
      evalStmt(stmt, res.vars, res.console)
    }

    result.console.getOutput should be("x is 5")
    result.vars("x") should be(5)
  }

  test("integer assignment with parsed code") {
    // Test with just the assignment portion
    val assignmentCode = "x <- 42"
    val Parsed.Success(assignmentStmt, _) = parse(assignmentCode, assignmentNoSymbols(_))

    val vars = VarMap("x" -> 0)
    val console = TestConsoleIO()

    val result = evalStmt(assignmentStmt, vars, console)

    result.vars("x") should be(42)
  }

  test("string assignment with parsed code") {
    // Test with just the assignment portion
    val assignmentCode = "message <- \"Hello\""
    val Parsed.Success(assignmentStmt, _) = parse(assignmentCode, assignmentNoSymbols(_))

    val vars = VarMap("message" -> "")
    val console = TestConsoleIO()

    val result = evalStmt(assignmentStmt, vars, console)

    result.vars("message") should be("Hello")
  }

  test("sequence of statements with assignment") {
    implicit val symbols: SymbolTable = SymbolTable(Map("x" -> IntType))
    // First parse just the assignment
    val assignmentCode = "x <- 42"
    val Parsed.Success(assignStmt, _) = parse(assignmentCode, assignment(_))

    // Then parse an if statement
    val ifCode =
      """Si x = 42 Alors
        |  Ecrire("x is 42\NL")
        |Fin Si""".stripMargin
    val Parsed.Success(ifStmt, _) = parse(ifCode, ifStatement(_))

    // Execute them in sequence
    val vars = VarMap("x" -> 0)
    val console = TestConsoleIO()

    val result1 = evalStmt(assignStmt, vars, console)
    val result2 = evalStmt(ifStmt, result1.vars, result1.console)

    result2.console.getOutput should be("x is 42\n")
    result2.vars("x") should be(42)
  }

  test("assign another variable") {
    val code = "Algorithme: test\nVariables:\ns0, s1: string\n" +
      "Début\ns1 <- s0\nFin"

    parse(code, program(_)) match {
      case Parsed.Success(program, index) =>
        val stmt = program.statements.head
        val vars = VarMap("s0" -> "hello", "s1" -> "")
        val result = evalStmt(stmt, vars)
        result.vars("s1") should be("hello")
      case Parsed.Failure(stack, idx, extra) =>
        fail(extra.trace().msg)
    }
  }

  test("multiple assignments with different types") {
    // Create a complete program with Variables section for context-aware parsing
    val code =
      """Algorithme: test
        |Variables:
        |  s0, s1: string
        |  i0, i1: entier
        |  b0, b1: booléen
        |Début
        |  Si Vrai Alors
        |    s1 <- s0 + " world"
        |    i1 <- i0 + 3
        |    b1 <- false OU b0
        |  Fin Si
        |Fin""".stripMargin

    parse(code, program(_)) match {
      case Parsed.Success(program, index) =>
        // Extract the if statement
        val stmt = program.statements.head
        val vars =
          VarMap("s0" -> "hello", "s1" -> "", "i0" -> 12, "i1" -> 0, "b0" -> true, "b1" -> false)
        val result = evalStmt(stmt, vars)
        result.vars("s1") should be("hello world")
        result.vars("i1") should be(15)
        result.vars("b1") shouldBe true

      case f @ Parsed.Failure(stack, idx, extra) =>
        fail(extra.trace().longTerminalsMsg)
    }
  }

  test("arithmetic operations") {
    implicit val symbols: SymbolTable = SymbolTable(Map("x" -> IntType, "y" -> IntType))
    val code = "x <- 2 + 3*(x+3) - y"
    val Parsed.Success(stmt, _) = parse(code, assignment(_))
    val result = evalStmt(stmt, VarMap("x" -> 2, "y" -> 4)).vars
    result should be(VarMap("x" -> 13, "y" -> 4))
  }

  test("boolean operations") {
    val code =
      """Algorithme: test
        |Variables:
        |  x: booléen
        |Début
        |  x <- false or true and (x or false)
        |Fin""".stripMargin

    parse(code, program(_)) match {
      case Parsed.Success(program, index) =>
        // Extract the assignment statement
        val stmt = program.statements.head
        var result = evalStmt(stmt, VarMap("x" -> true)).vars
        result should be(VarMap("x" -> true))
        result = evalStmt(stmt, VarMap("x" -> false)).vars
        result should be(VarMap("x" -> false))
      case Parsed.Failure(stack, idx, extra) =>
        fail(extra.trace().msg)
    }
  }

  test("array literal assignment") {
    val code = 
      """Algorithme: test
        |Variables:
        |  arr: arrayint
        |Début
        |  arr <- {1, 2, 3, 4, 5}
        |Fin""".stripMargin

    parse(code, program(_)) match {
      case Parsed.Success(program, index) =>
        val stmt = program.statements.head
        val vars = VarMap("arr" -> Array.empty[Int])
        val result = evalStmt(stmt, vars)
        val resultArray = result.vars("arr").asInstanceOf[Array[Int]]
        resultArray should be(Array(1, 2, 3, 4, 5))
      case Parsed.Failure(stack, idx, extra) =>
        fail(extra.trace().msg)
    }
  }

  test("array access") {
    val code = 
      """Algorithme: test
        |Variables:
        |  arr: arrayint
        |  x: entier
        |Début
        |  arr <- {10, 20, 30, 40, 50}
        |  x <- arr[2]
        |Fin""".stripMargin

    parse(code, program(_)) match {
      case Parsed.Success(program, index) =>
        val vars = VarMap("arr" -> Array.empty[Int], "x" -> 0)
        val result = program.statements.foldLeft(EvalResult(TestConsoleIO(), vars)) { (res, stmt) =>
          evalStmt(stmt, res.vars, res.console)
        }
        result.vars("x") should be(20) // Arrays are 1-indexed, so arr[2] = 20
      case Parsed.Failure(stack, idx, extra) =>
        fail(extra.trace().msg)
    }
  }

  test("array with variable assignment") {
    val code = 
      """Algorithme: test
        |Variables:
        |  source: arrayint
        |  target: arrayint
        |Début
        |  source <- {100, 200, 300}
        |  target <- source
        |Fin""".stripMargin

    parse(code, program(_)) match {
      case Parsed.Success(program, index) =>
        val vars = VarMap("source" -> Array.empty[Int], "target" -> Array.empty[Int])
        val result = program.statements.foldLeft(EvalResult(TestConsoleIO(), vars)) { (res, stmt) =>
          evalStmt(stmt, res.vars, res.console)
        }
        val targetArray = result.vars("target").asInstanceOf[Array[Int]]
        targetArray should be(Array(100, 200, 300))
      case Parsed.Failure(stack, idx, extra) =>
        fail(extra.trace().msg)
    }
  }

  test("array access in arithmetic expression") {
    val code = 
      """Algorithme: test
        |Variables:
        |  arr: arrayint
        |  result: entier
        |Début
        |  arr <- {5, 10, 15, 20}
        |  result <- arr[1] + arr[3] * 2
        |Fin""".stripMargin

    parse(code, program(_)) match {
      case Parsed.Success(program, index) =>
        val vars = VarMap("arr" -> Array.empty[Int], "result" -> 0)
        val result = program.statements.foldLeft(EvalResult(TestConsoleIO(), vars)) { (res, stmt) =>
          evalStmt(stmt, res.vars, res.console)
        }
        result.vars("result") should be(35) // arr[1] + arr[3] * 2 = 5 + 15 * 2 = 35
      case Parsed.Failure(stack, idx, extra) =>
        fail(extra.trace().msg)
    }
  }

  test("array access with variable index") {
    val code = 
      """Algorithme: test
        |Variables:
        |  arr: arrayint
        |  index: entier
        |  value: entier
        |Début
        |  arr <- {7, 14, 21, 28, 35}
        |  index <- 3
        |  value <- arr[index]
        |Fin""".stripMargin

    parse(code, program(_)) match {
      case Parsed.Success(program, index) =>
        val vars = VarMap("arr" -> Array.empty[Int], "index" -> 0, "value" -> 0)
        val result = program.statements.foldLeft(EvalResult(TestConsoleIO(), vars)) { (res, stmt) =>
          evalStmt(stmt, res.vars, res.console)
        }
        result.vars("value") should be(21) // arr[3] = 21
      case Parsed.Failure(stack, idx, extra) =>
        fail(extra.trace().msg)
    }
  }

  test("empty array literal") {
    val code = 
      """Algorithme: test
        |Variables:
        |  arr: arrayint
        |Début
        |  arr <- {}
        |Fin""".stripMargin

    parse(code, program(_)) match {
      case Parsed.Success(program, index) =>
        val stmt = program.statements.head
        val vars = VarMap("arr" -> Array.empty[Int])
        val result = evalStmt(stmt, vars)
        val resultArray = result.vars("arr").asInstanceOf[Array[Int]]
        resultArray should be(Array.empty[Int])
      case Parsed.Failure(stack, idx, extra) =>
        fail(extra.trace().msg)
    }
  }

  test("array type checking - compatible assignment") {
    val vars = VarMap("arr" -> Array.empty[Int])
    val assignment = Assignment("arr", ArrayLiteral(Seq(IntLiteral(1), IntLiteral(2), IntLiteral(3))))
    val result = evalStmt(assignment, vars, TestConsoleIO())

    val resultArray = result.vars("arr").asInstanceOf[Array[Int]]
    resultArray should be(Array(1, 2, 3))
    result.console.getOutput should be("")
  }

  test("array type checking - incompatible assignment") {
    val vars = VarMap("x" -> 0) // x is an int, not an array
    val assignment = Assignment("x", ArrayLiteral(Seq(IntLiteral(1), IntLiteral(2))))

    val exception = intercept[RuntimeException] {
      evalStmt(assignment, vars, TestConsoleIO())
    }

    exception.getMessage should include("Type error")
    exception.getMessage should include("Array")
    exception.getMessage should include("x")
  }

  test("array declaration with size syntax") {
    val code = 
      """Algorithme: test
        |Variables:
        |  arr [5] : tableau d'entier
        |  x: entier
        |Début
        |  arr <- {1, 2, 3, 4, 5}
        |  x <- arr[2]
        |Fin""".stripMargin

    parse(code, program(_)) match {
      case Parsed.Success(program, index) =>
        val vars = VarMap("arr" -> Array.empty[Int], "x" -> 0)
        val result = program.statements.foldLeft(EvalResult(TestConsoleIO(), vars)) { (res, stmt) =>
          evalStmt(stmt, res.vars, res.console)
        }
        result.vars("x") should be(2) // arr[2] = 2
      case Parsed.Failure(stack, idx, extra) =>
        fail(extra.trace().msg)
    }
  }

  test("array size is respected in initialization") {
    val code = 
      """Algorithme: test
        |Variables:
        |  arr [7] : tableau d'entier
        |Début
        |Fin""".stripMargin

    val result = PseudoInterpreter.run(code).value
    val array = result.vars("arr").asInstanceOf[Array[Int]]
    array.length should be(7)
    array should be(Array(0, 0, 0, 0, 0, 0, 0)) // Should be filled with zeros
  }

  test("read a string variable from stdin") {
    val vars = PseudoInterpreter.evalStmt(
      FunctionCall("read", Seq(StringRef("s"))),
      VarMap("s" -> ""),
      TestConsoleIO(input = "hello world")
    ).vars
    vars("s") shouldBe("hello world")
  }

  test("read an int variable from stdin") {
    val vars = PseudoInterpreter.evalStmt(
      FunctionCall("read", Seq(IntRef("i"))),
      VarMap("i" -> 0),
      TestConsoleIO(input = "12")
    ).vars
    vars("i") shouldBe (12)
  }

  test("read multiple variables") {
    val code =
      """Algorithme: test
        |Variables:
        |  s: chaine
        |  i: entier
        |  b: booléen
        |Début
        |  Lire(s)
        |  Lire(i)
        |  Lire(b)
        |Fin""".stripMargin

    val result = PseudoInterpreter.run(code, TestConsoleIO(input = "hello\n24\ntrue\n")).value
    result.vars("s") shouldBe("hello")
    result.vars("i") shouldBe(24)
    result.vars("b") shouldBe(true)

  }

  test("while loop - basic counting"):
    implicit val symbols: SymbolTable = SymbolTable(Map("x" -> IntType))
    val code =
      """Tant que x < 5 Faire
        |  Ecrire("x: ", x, "\NL")
        |  x <- x + 1
        |Fin Tant que""".stripMargin
    val stmt = parse(code, statement(_)).get.value
    val result = evalStmt(stmt, VarMap("x" -> 0), TestConsoleIO())
    
    result.console.getOutput shouldBe "x: 0\nx: 1\nx: 2\nx: 3\nx: 4\n"
    result.vars("x") shouldBe 5

  test("while loop - condition false from start"):
    implicit val symbols: SymbolTable = SymbolTable(Map("x" -> IntType))
    val code =
      """Tant que x < 0 Faire
        |  Ecrire("This should not print\NL")
        |Fin Tant que""".stripMargin
    val stmt = parse(code, statement(_)).get.value
    val result = evalStmt(stmt, VarMap("x" -> 5), TestConsoleIO())
    
    result.console.getOutput shouldBe ""
    result.vars("x") shouldBe 5

  test("while loop in complete program"):
    val code =
      """Algorithme: countdown
        |Variables:
        |  counter: entier
        |Début
        |  counter <- 3
        |  Tant que counter > 0 Faire
        |    Ecrire("Countdown: ", counter, "\NL")
        |    counter <- counter - 1
        |  Fin Tant que
        |  Ecrire("Done!\NL")
        |Fin""".stripMargin

    val result = PseudoInterpreter.run(code, TestConsoleIO()).value
    result.console.getOutput shouldBe "Countdown: 3\nCountdown: 2\nCountdown: 1\nDone!\n"
    result.vars("counter") shouldBe 0

