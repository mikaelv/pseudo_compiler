package pseudoc

import org.scalatest.funsuite.AnyFunSuiteLike
import PseudoCodeParser.*
import fastparse.*
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers.*
import pseudoc.Ast.{Algorithm, Assignment, BooleanExpression, Comparison, ComparisonOperator, ForLoop, FunctionCall, IfStatement, IntAddSub, IntAssignment, IntLiteral, IntMultDiv, IntRef, Statement, StringAssignment, StringConcat, StringLiteral, StringRef, VariableDecl, Variables}

class PseudoCodeParserTest extends AnyFunSuiteLike:
  def check[A](
      str: String,
      myParser: P[_] => P[A],
      expectedValue: A
  ): Assertion =
    val result = parse(str, myParser(_))
    result.get.value should ===(expectedValue)

  test("algorithme"):
    check(
      "Algorithme: recherche_dichotomique1",
      algo(_),
      Algorithm("recherche_dichotomique1")
    )
    check("Algorithme  :algo2", algo(_), Algorithm("algo2"))

  test("string variable"):
    check("chaine1: chaîne", variableDecl(_), VariableDecl("chaine1", "string"))

  test("variables"):
    check(
      "Variables:\nchaine1: string",
      variables(_),
      Variables(Seq(VariableDecl("chaine1", "string")))
    )

    check(
      "Variables:\n chaine1: chaîne, chaine2: chaîne de caractères, chaine3: string",
      variables(_),
      Variables(
        Seq(
          VariableDecl("chaine1", "string"),
          VariableDecl("chaine2", "string"),
          VariableDecl("chaine3", "string")
        )
      )
    )

  test("for loop"):
    check(
      "Pour i <- 1 à 10 Faire\nFin Pour",
      forLoop(_),
      ForLoop("i", 1, 10, Seq())
    )

  test("write"):
    check(
      "Write(a + \"hello\" + b + \"world\")",
      print(_),
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
      Comparison(IntRef("x"), ComparisonOperator.GreaterThanEqual, IntLiteral(5))
    )
    
  test("if statement - no else"):
    check(
      "Si x = 5 Alors\nEcrire(\"x is 5\")\nFin Si",
      ifStatement(_),
      IfStatement(
        Comparison(IntRef("x"), ComparisonOperator.Equal, IntLiteral(5)),
        Seq(FunctionCall("print", Seq(StringConcat(Seq(StringLiteral("x is 5")))))),
        None
      )
    )
    
  test("if statement - with else"):
    check(
      "Si x = 5 Alors\nEcrire(\"x is 5\")\nSinon\nEcrire(\"x is not 5\")\nFin Si",
      ifStatement(_),
      IfStatement(
        Comparison(IntRef("x"), ComparisonOperator.Equal, IntLiteral(5)),
        Seq(FunctionCall("print", Seq(StringConcat(Seq(StringLiteral("x is 5")))))),
        Some(Seq(FunctionCall("print", Seq(StringConcat(Seq(StringLiteral("x is not 5")))))))
      )
    )
    
  test("if statement - english syntax"):
    check(
      "If x = 5 Then\nPrint(\"x is 5\")\nElse\nPrint(\"x is not 5\")\nEnd If",
      ifStatement(_),
      IfStatement(
        Comparison(IntRef("x"), ComparisonOperator.Equal, IntLiteral(5)),
        Seq(FunctionCall("print", Seq(StringConcat(Seq(StringLiteral("x is 5")))))),
        Some(Seq(FunctionCall("print", Seq(StringConcat(Seq(StringLiteral("x is not 5")))))))
      )
    )
    
  test("variable assignment with integer"):
    check(
      "x <- 42",
      assignment(_),
      IntAssignment("x", IntAddSub(IntMultDiv(IntLiteral(42), Seq.empty), Seq.empty))
    )
    
  test("variable assignment with string"):
    check(
      "message <- \"Hello, world!\"",
      assignment(_),
      StringAssignment("message", StringConcat(Seq(StringLiteral("Hello, world!"))))
    )
    
  test("variable assignment with concatenation"):
    check(
      "greeting <- \"Hello, \" + name",
      assignment(_),
      StringAssignment("greeting", StringConcat(Seq(StringLiteral("Hello, "), StringRef("name"))))
    )