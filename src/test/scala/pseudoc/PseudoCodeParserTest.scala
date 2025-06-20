package pseudoc

import org.scalatest.funsuite.AnyFunSuiteLike
import PseudoCodeParser.*
import fastparse.*
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers.*
import pseudoc.BooleanExpressionParser.comparisonExpr
import pseudoc.Lexical.identifier
import pseudoc.ast.*
import pseudoc.PseudoType
import pseudoc.PseudoType.{ArrayIntType, BoolType, IntType, StringType}
import pseudoc.ast.AddSubOperator.{Add, Sub}
import pseudoc.ast.MultDivOperator.{Div, Mult}

class PseudoCodeParserTest extends AnyFunSuiteLike:
  def check[A](
      str: String,
      myParser: P[_] => P[A],
      expectedValue: A
  ): Assertion =
    val result = parse(str, myParser(_))
    result.get.value should ===(expectedValue)

  def assignmentNoSymbols[$: P]: P[Assignment] =
    PseudoCodeParser.assignment(symbols = SymbolTable())

  test("identifier"):
    check("test123 \nblah :\n", identifier(_), "test123")

  test("arithmetic expression"):
    implicit val symbols: SymbolTable = SymbolTable(Map("x" -> IntType, "y" -> IntType))
    check(
      "-234 +  -3*12 - x/2*y",
      expression(_),
      IntAddSub(
        IntLiteral(-234),
        Seq(
          (Add, IntMultDiv(IntLiteral(-3), Seq(Mult -> IntLiteral(12)))),
          (Sub, IntMultDiv(IntRef("x"), Seq(Div -> IntLiteral(2), Mult -> IntRef("y"))))
        )
      )
    )

  test("large integer literal"):
    implicit val symbols: SymbolTable = SymbolTable()
    check("2025", IntExpressionParser.intLiteral(_), IntLiteral(2025))
    check("12345", IntExpressionParser.intLiteral(_), IntLiteral(12345))
    check("-2025", IntExpressionParser.intLiteral(_), IntLiteral(-2025))

  test("assignment with large integer"):
    implicit val symbols: SymbolTable = SymbolTable(Map("x" -> IntType))
    check(
      "x <- 2025",
      assignment(_),
      Assignment("x", IntLiteral(2025))
    )

  test("complete program with large integer"):
    val code = """Algorithme: test_large_int
Variables:
  x : entier
  y : entier
Debut
  x <- 25
  y <- 2025
  Ecrire("x = ", x, "\NL")
  Ecrire("y = ", y, "\NL")
Fin"""
    val result = parse(code, program(_))
    result.isSuccess should be(true)
    val prog = result.get.value
    prog.statements should have length 4  // 2 assignments + 2 prints


  test("large integer evaluation"):
    val code = """Algorithme: test_eval
Variables:
  x : entier
Debut
  x <- 2025
  Ecrire("Value: ", x, "\NL")
Fin"""
    
    val console = TestConsoleIO()
    val result = PseudoInterpreter.run(code, console)
    result.isRight should be(true)
    result.toOption.get.console.getOutput should include("2025")

  test("algorithme"):
    check(
      "Algorithme: recherche_dichotomique1",
      algo(_),
      Algorithm("recherche_dichotomique1")
    )
    check("Algorithme  :algo2", algo(_), Algorithm("algo2"))

  test("string variable"):
    check(
      "chaine1: chaîne",
      variableDecl(_),
      Variables(Seq(VariableDecl("chaine1", PseudoType.StringType)))
    )

  test("variables on one line"):
    check(
      "Variables:\nchaine1: string",
      variables(_),
      Variables(Seq(VariableDecl("chaine1", PseudoType.StringType)))
    )

    check(
      "Variables:\nchaine1, chaine2: string",
      variables(_),
      Variables(
        Seq(
          VariableDecl("chaine1", PseudoType.StringType),
          VariableDecl("chaine2", PseudoType.StringType)
        )
      )
    )

  test("variables on multiple lines"):
    check(
      "Variables:\nchaine1, chaine2: string \r\n i1, i2: entier",
      variables(_),
      Variables(
        Seq(
          VariableDecl("chaine1", PseudoType.StringType),
          VariableDecl("chaine2", PseudoType.StringType),
          VariableDecl("i1", PseudoType.IntType),
          VariableDecl("i2", PseudoType.IntType)
        )
      )
    )

  test("program") {
    check(
      "Algorithme: test \n" +
        "Variables :\n" +
        "  i: int\n" +
        "Début\n" +
        "  i <- 1\n" +
        "Fin",
      program(_),
      Program(
        Algorithm("test"),
        Variables(Seq(VariableDecl("i", IntType))),
        Seq(Assignment("i", IntLiteral(1)))
      )
    )
  }

  test("for loop"):
    implicit val symbols: SymbolTable = SymbolTable(Map("i" -> IntType))
    check(
      "Pour i <- 1 à 10 Faire\nFin Pour",
      forLoop(_),
      ForLoop("i", IntLiteral(1), IntLiteral(10), Seq())
    )

  test("write"):
    implicit val symbols: SymbolTable = SymbolTable(Map("a" -> StringType, "b" -> StringType))
    check(
      "Write(a + \"hello\" + b + \"world\")",
      StringExpressionParser.print(_),
      FunctionCall(
        "print",
        Seq(
          StringConcat(
            Seq(
              StringRef("a"),
              StringLiteral("hello"),
              StringRef("b"),
              StringLiteral("world")
            )
          )
        )
      )
    )

  test("comparison expression"):
    implicit val symbols: SymbolTable = SymbolTable(Map("x" -> IntType))
    check(
      "x = 5",
      comparisonExpr(_),
      Comparison(IntRef("x"), ComparisonOperator.Equal, IntLiteral(5))
    )

    check(
      "x != 5",
      comparisonExpr(_),
      Comparison(IntRef("x"), ComparisonOperator.NotEqual, IntLiteral(5))
    )

    check(
      "x < 5",
      comparisonExpr(_),
      Comparison(IntRef("x"), ComparisonOperator.LessThan, IntLiteral(5))
    )

    check(
      "x > 5",
      comparisonExpr(_),
      Comparison(IntRef("x"), ComparisonOperator.GreaterThan, IntLiteral(5))
    )

    check(
      "x <= 5",
      comparisonExpr(_),
      Comparison(IntRef("x"), ComparisonOperator.LessThanEqual, IntLiteral(5))
    )

    check(
      "x >= 5",
      comparisonExpr(_),
      Comparison(
        IntRef("x"),
        ComparisonOperator.GreaterThanEqual,
        IntLiteral(5)
      )
    )

  test("print a string, int, bool"):
    implicit val symbols: SymbolTable =
      SymbolTable(Map("s" -> StringType, "i" -> IntType, "b" -> BoolType))
    check(
      """print("s: ", s, " i: ", i, " b: ", b)""",
      statement(_),
      FunctionCall(
        "print",
        List(
          StringLiteral("s: "),
          StringRef("s"),
          StringLiteral(" i: "),
          IntRef("i"),
          StringLiteral(" b: "),
          BoolRef("b")
        )
      )
    )

  test("if statement - no else"):
    implicit val symbols: SymbolTable = SymbolTable(Map("x" -> IntType))
    check(
      "Si x = 5 Alors\nEcrire(\"x is 5\")\nFin Si",
      ifStatement(_),
      IfStatement(
        Comparison(IntRef("x"), ComparisonOperator.Equal, IntLiteral(5)),
        Seq(
          FunctionCall(
            "print",
            Seq(StringConcat(Seq(StringLiteral("x is 5"))))
          )
        ),
        None
      )
    )

  test("if statement - with else"):
    implicit val symbols: SymbolTable = SymbolTable(Map("x" -> IntType))
    check(
      "Si x = 5 Alors\nEcrire(\"x is 5\")\nSinon\nEcrire(\"x is not 5\")\nFin Si",
      ifStatement(_),
      IfStatement(
        Comparison(IntRef("x"), ComparisonOperator.Equal, IntLiteral(5)),
        Seq(
          FunctionCall(
            "print",
            Seq(StringConcat(Seq(StringLiteral("x is 5"))))
          )
        ),
        Some(
          Seq(
            FunctionCall(
              "print",
              Seq(StringConcat(Seq(StringLiteral("x is not 5"))))
            )
          )
        )
      )
    )

  test("if statement - english syntax"):
    implicit val symbols: SymbolTable = SymbolTable(Map("x" -> IntType))
    check(
      "If x = 5 Then\nPrint(\"x is 5\")\nElse\nPrint(\"x is not 5\")\nEnd If",
      ifStatement(_),
      IfStatement(
        Comparison(IntRef("x"), ComparisonOperator.Equal, IntLiteral(5)),
        Seq(
          FunctionCall(
            "print",
            Seq(StringConcat(Seq(StringLiteral("x is 5"))))
          )
        ),
        Some(
          Seq(
            FunctionCall(
              "print",
              Seq(StringConcat(Seq(StringLiteral("x is not 5"))))
            )
          )
        )
      )
    )

  test("variable assignment with integer"):
    check(
      "x <- -42",
      assignmentNoSymbols(_),
      Assignment("x", IntLiteral(-42))
    )

  test("variable assignment with string"):
    implicit val symbols: SymbolTable = SymbolTable()
    check(
      "message <- \"Hello, world!\"",
      assignmentNoSymbols(_),
      Assignment(
        "message",
        StringConcat(Seq(StringLiteral("Hello, world!")))
      )
    )

  test("variable assignment with concatenation"):
    implicit val symbols: SymbolTable = SymbolTable(Map("name" -> StringType))
    check(
      "greeting <- \"Hello, \" + name",
      assignment(_),
      Assignment(
        "greeting",
        StringConcat(Seq(StringLiteral("Hello, "), StringRef("name")))
      )
    )

  test("boolean expression"):
    implicit val symbols: SymbolTable = SymbolTable(Map("x" -> BoolType))
    check(
      "true or x",
      BooleanExpressionParser.boolExpr(_),
      BoolOperations(BoolLiteral(true), (BooleanOperator.Or, BoolRef("x")))
    )

    check(
      "x or true",
      BooleanExpressionParser.boolExpr(_),
      BoolOperations(BoolRef("x"), (BooleanOperator.Or, BoolLiteral(true)))
    )

    check(
      "false or true",
      BooleanExpressionParser.boolExpr(_),
      BoolOperations(BoolLiteral(false), (BooleanOperator.Or, BoolLiteral(true)))
    )

    check(
      "false or true and (x or false)",
      BooleanExpressionParser.boolExpr(_),
      BoolOperations(
        BoolLiteral(false),
        (
          BooleanOperator.Or,
          BoolOperations(
            BoolLiteral(true),
            (
              BooleanOperator.And,
              BoolOperations(BoolRef("x"), (BooleanOperator.Or, BoolLiteral(false)))
            )
          )
        )
      )
    )

  test("boolean assignment") {
    implicit val symbols: SymbolTable = SymbolTable(Map("x" -> BoolType))
    check(
      "x <- false or true and (x or false)",
      PseudoCodeParser.assignment(_),
      Assignment(
        "x",
        BoolOperations(
          BoolLiteral(false),
          (
            BooleanOperator.Or,
            BoolOperations(
              BoolLiteral(true),
              (
                BooleanOperator.And,
                BoolOperations(BoolRef("x"), (BooleanOperator.Or, BoolLiteral(false)))
              )
            )
          )
        )
      )
    )
  }

  test("array literal parsing") {
    implicit val symbols: SymbolTable = SymbolTable(Map("arr" -> ArrayIntType))
    check(
      "arr <- {1, 2, 3, 4, 5}",
      assignment(_),
      Assignment(
        "arr",
        ArrayLiteral(Seq(IntLiteral(1), IntLiteral(2), IntLiteral(3), IntLiteral(4), IntLiteral(5)))
      )
    )
  }

  test("empty array literal parsing") {
    implicit val symbols: SymbolTable = SymbolTable(Map("arr" -> ArrayIntType))
    check(
      "arr <- {}",
      assignment(_),
      Assignment("arr", ArrayLiteral(Seq()))
    )
  }

  test("array access expression parsing") {
    implicit val symbols: SymbolTable = SymbolTable(Map("arr" -> ArrayIntType))
    check(
      "arr[2]",
      IntExpressionParser.arrayAccess(_),
      ArrayAccess(ArrayRef("arr"), IntLiteral(2))
    )
  }

  test("array access in integer expression") {
    implicit val symbols: SymbolTable = SymbolTable(Map("arr" -> ArrayIntType))
    check(
      "arr[2]",
      IntExpressionParser.intExpr(_),
      ArrayAccess(ArrayRef("arr"), IntLiteral(2))
    )
  }

  test("array access assignment") {
    implicit val symbols: SymbolTable = SymbolTable(Map("arr" -> ArrayIntType, "x" -> IntType))
    check(
      "x <- arr[2]",
      assignment(_),
      Assignment("x", ArrayAccess(ArrayRef("arr"), IntLiteral(2)))
    )
  }

  test("array access with variable index") {
    implicit val symbols: SymbolTable =
      SymbolTable(Map("arr" -> ArrayIntType, "index" -> IntType, "x" -> IntType))
    check(
      "x <- arr[index]",
      assignment(_),
      Assignment("x", ArrayAccess(ArrayRef("arr"), IntRef("index")))
    )
  }

  test("array access in arithmetic expression") {
    implicit val symbols: SymbolTable = SymbolTable(Map("arr" -> ArrayIntType, "result" -> IntType))
    check(
      "result <- arr[1] + arr[3] * 2",
      assignment(_),
      Assignment(
        "result",
        IntAddSub(
          ArrayAccess(ArrayRef("arr"), IntLiteral(1)),
          Seq(
            (
              AddSubOperator.Add,
              IntMultDiv(
                ArrayAccess(ArrayRef("arr"), IntLiteral(3)),
                Seq((MultDivOperator.Mult, IntLiteral(2)))
              )
            )
          )
        )
      )
    )
  }

  test("array variable declaration with size") {
    check(
      "arr [10] : tableau d'entier",
      arrayVariableDecl(_),
      Variables(Seq(ArrayVariableDecl("arr", ArrayIntType, 10)))
    )

    check(
      "myArray[5]: array of integers",
      arrayVariableDecl(_),
      Variables(Seq(ArrayVariableDecl("myArray", ArrayIntType, 5)))
    )

    check(
      "myArray  [5]  :  array of integer",
      arrayVariableDecl(_),
      Variables(Seq(ArrayVariableDecl("myArray", ArrayIntType, 5)))
    )

    check(
      "myArray[5]   :array of integers",
      arrayVariableDecl(_),
      Variables(Seq(ArrayVariableDecl("myArray", ArrayIntType, 5)))
    )
  }

  test("variables section with array declaration") {
    check(
      "Variables:\narr [10] : tableau d'entier",
      variables(_),
      Variables(Seq(ArrayVariableDecl("arr", ArrayIntType, 10)))
    )
  }

  test("read input") {
    implicit val symbols: SymbolTable = SymbolTable(Map("s" -> StringType, "i" -> IntType))
    check(
      "Lire(s)",
      statement(_),
      FunctionCall("read", Seq(StringRef("s")))
    )

    check(
      "Lire(i)",
      statement(_),
      FunctionCall("read", Seq(IntRef("i")))
    )
  }

  test("while loop - French syntax"):
    implicit val symbols: SymbolTable = SymbolTable(Map("x" -> IntType))
    check(
      "Tant que x < 10 Faire\nFin Tant que",
      whileLoop(_),
      WhileLoop(
        Comparison(IntRef("x"), ComparisonOperator.LessThan, IntLiteral(10)),
        Seq()
      )
    )

  test("while loop - English syntax"):
    implicit val symbols: SymbolTable = SymbolTable(Map("x" -> IntType))
    check(
      "While x < 10 do\nEnd While",
      whileLoop(_),
      WhileLoop(
        Comparison(IntRef("x"), ComparisonOperator.LessThan, IntLiteral(10)),
        Seq()
      )
    )

  test("while loop with statements"):
    implicit val symbols: SymbolTable = SymbolTable(Map("x" -> IntType))
    check(
      "Tant que x < 10 Faire\nx <- x + 1\nprint(x)\nFin Tant que",
      whileLoop(_),
      WhileLoop(
        Comparison(IntRef("x"), ComparisonOperator.LessThan, IntLiteral(10)),
        Seq(
          Assignment("x", IntAddSub(IntRef("x"), Seq((Add, IntLiteral(1))))),
          FunctionCall("print", Seq(IntRef("x")))
        )
      )
    )
