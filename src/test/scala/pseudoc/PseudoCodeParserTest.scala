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
import pseudoc.PseudoType.{BoolType, IntType, StringType}

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

  test("algorithme"):
    check(
      "Algorithme: recherche_dichotomique1",
      algo(_),
      Algorithm("recherche_dichotomique1")
    )
    check("Algorithme  :algo2", algo(_), Algorithm("algo2"))

  test("string variable"):
    check("chaine1: chaîne", variableDecl(_),
      Variables(Seq(VariableDecl("chaine1", PseudoType.StringType))))

  test("variables"):
    check(
      "Variables:\nchaine1: string",
      variables(_),
      Variables(Seq(VariableDecl("chaine1", PseudoType.StringType)))
    )

    check(
      "Variables:\nchaine1, chaine2: string",
      variables(_),
      Variables(Seq(
        VariableDecl("chaine1", PseudoType.StringType),
        VariableDecl("chaine2", PseudoType.StringType)
      ))
    )


    check(
      "Variables:\n chaine1: chaîne, i: entier, chaine3: string",
      variables(_),
      Variables(
        Seq(
          VariableDecl("chaine1", PseudoType.StringType),
          VariableDecl("i", PseudoType.IntType),
          VariableDecl("chaine3", PseudoType.StringType)
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
      "x <- 42",
      assignmentNoSymbols(_),
      Assignment("x", IntLiteral(42))
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
