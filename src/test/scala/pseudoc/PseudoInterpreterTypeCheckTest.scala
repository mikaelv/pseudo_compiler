package pseudoc

import fastparse.P
import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.*
import pseudoc.ast.*
import pseudoc.PseudoType

class PseudoInterpreterTypeCheckTest extends AnyWordSpec with Matchers {
  def assignment[$: P]: P[Assignment] = PseudoCodeParser.assignment(symbols = SymbolTable())


  "PseudoInterpreter with type checking" should {
    "successfully parse, type check and execute valid programs" in {
      // Create a simple program for testing
      val algo = Algorithm("TestAlgo")
      val vars = Variables(Seq(
        VariableDecl("x", PseudoType.IntType),
        VariableDecl("message", PseudoType.StringType)
      ))
      
      val statements = Seq(
        Assignment("x", IntLiteral(42)),
        Assignment("message", StringLiteral("Hello")),
        FunctionCall("print", Seq(StringRef("message")))
      )
      
      val program = Program(algo, vars, statements)
      
      // Type checking should pass
      val typeCheckResult = program.typeCheck()
      typeCheckResult shouldBe a[Right[_, _]]
      
      // Manually evaluate the program
      val initialVarMap = VarMap(Map(
        "x" -> (classOf[Int], 0),
        "message" -> (classOf[String], "")
      ))
      
      val testConsole = TestConsoleIO()
      val result = statements.foldLeft(EvalResult(testConsole, initialVarMap)) { 
        (result, statement) => PseudoInterpreter.evalStmt(statement, result.vars, result.console)
      }
      
      // Check console output
      result.console.getOutput should include("Hello")
      
      // Check variables
      result.vars.apply("x") shouldBe 42
      result.vars.apply("message") shouldBe "Hello"
    }
    
    "report parse errors" in {
      // This test needs to use the parser since we're testing parsing errors
      import fastparse._
      
      // Simulate a parsing error with an incomplete expression
      val invalidInput = "x <- "
      
      // Try to parse just this expression
      val parseResult = parse(invalidInput, assignment(_))
      
      // Should be a parse failure
      parseResult match {
        case s: Parsed.Success[_] => fail("Expected parsing to fail but it succeeded with " + s)
        case _: Parsed.Failure => succeed // Expected outcome
      }
    }
    
    "report type errors" in {
      // Create a program with a type error
      val algo = Algorithm("TestAlgo")
      val vars = Variables(Seq(
        VariableDecl("x", PseudoType.IntType),
        VariableDecl("message", PseudoType.StringType)
      ))
      
      val statements = Seq(
        // Type error: assigning string literal to integer variable
        Assignment("x", IntRef("message")),
        Assignment("message", StringLiteral("Hello"))
      )
      
      val program = Program(algo, vars, statements)
      
      // Type checking should fail
      val typeCheckResult = program.typeCheck()
      typeCheckResult shouldBe a[Left[_, _]]
      typeCheckResult.left.get should include("Type mismatch")
    }
    
    "handle complex programs with correct types" in {
      // Create a program with complex but type-correct operations
      val algo = Algorithm("ComplexTest")
      val vars = Variables(Seq(
        VariableDecl("sum", PseudoType.IntType),
        VariableDecl("flag", PseudoType.BoolType),
        VariableDecl("message", PseudoType.StringType)
      ))
      
      val statements = Seq(
        // Initialize variables
        Assignment("sum", IntLiteral(0)),
        Assignment("flag", BoolLiteral(true)),
        
        // Integer arithmetic
        Assignment("sum", IntAddSub(
          IntMultDiv(IntRef("sum"), Seq()),
          Seq((AddSubOperator.Add, IntMultDiv(IntLiteral(10), Seq())))
        )),
        
        // Boolean comparison
        Assignment("flag", Comparison(
          IntRef("sum"),
          ComparisonOperator.GreaterThan,
          IntLiteral(5)
        )),
        
        // String assignment without concatenating with int
        Assignment("message", StringLiteral("Result verified"))
      )
      
      val program = Program(algo, vars, statements)
      
      // Type checking should pass
      val typeCheckResult = program.typeCheck()
      typeCheckResult shouldBe a[Right[_, _]]
      
      // Setup for evaluation
      val initialVarMap = VarMap(Map(
        "sum" -> (classOf[Int], 0),
        "flag" -> (classOf[Boolean], false),
        "message" -> (classOf[String], "")
      ))
      
      val testConsole = TestConsoleIO()
      
      // Evaluate the program
      val result = statements.foldLeft(EvalResult(testConsole, initialVarMap)) { 
        (current, statement) => PseudoInterpreter.evalStmt(statement, current.vars, current.console)
      }
      
      // Check final variable values
      result.vars.apply("sum") shouldBe 10
      result.vars.apply("flag") shouldBe true
      result.vars.apply("message") shouldBe "Result verified"
    }
    
    "detect errors in conditional expressions" in {
      // Create a program with an invalid comparison
      val algo = Algorithm("ConditionTest")
      val vars = Variables(Seq(
        VariableDecl("x", PseudoType.IntType),
        VariableDecl("y", PseudoType.StringType)
      ))
      
      val statements = Seq(
        Assignment("x", IntLiteral(10)),
        Assignment("y", StringLiteral("hello")),
        // Type error: attempting to use string in comparison
        IfStatement(
          Comparison(
            IntRef("y"),  // Error: y is a string, not an int
            ComparisonOperator.GreaterThan,
            IntLiteral(5)
          ),
          Seq(FunctionCall("print", Seq(StringLiteral("This won't work"))))
        )
      )
      
      val program = Program(algo, vars, statements)
      
      // Type checking should fail
      val typeCheckResult = program.typeCheck()
      typeCheckResult shouldBe a[Left[_, _]]
      typeCheckResult.left.get should include("Type mismatch")
    }
    
    "validate variable declarations and initializations" in {
      // Create a program with correct variable initializations
      val algo = Algorithm("VarInitTest")
      val vars = Variables(Seq(
        VariableDecl("a", PseudoType.IntType),
        VariableDecl("b", PseudoType.StringType),
        VariableDecl("c", PseudoType.BoolType)
      ))
      
      val statements = Seq(
        Assignment("a", IntLiteral(42)),
        Assignment("b", StringLiteral("Hello")),
        Assignment("c", BoolLiteral(true)),
        // Print only string value (no concatenation with int)
        FunctionCall("print", Seq(StringRef("b")))
      )
      
      val program = Program(algo, vars, statements)
      
      // Type checking should pass
      val typeCheckResult = program.typeCheck()
      typeCheckResult shouldBe a[Right[_, _]]
      
      // Setup for evaluation
      val initialVarMap = VarMap(Map(
        "a" -> (classOf[Int], 0),
        "b" -> (classOf[String], ""),
        "c" -> (classOf[Boolean], false)
      ))
      
      val testConsole = TestConsoleIO()
      
      // Evaluate the program
      val result = statements.foldLeft(EvalResult(testConsole, initialVarMap)) { 
        (current, statement) => PseudoInterpreter.evalStmt(statement, current.vars, current.console)
      }
      
      // Check console output
      result.console.getOutput should include("Hello")
      
      // Check variables
      result.vars.apply("a") shouldBe 42
      result.vars.apply("b") shouldBe "Hello"
      result.vars.apply("c") shouldBe true
    }
  }
}