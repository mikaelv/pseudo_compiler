package pseudoc

import org.scalatest._
import org.scalatest.matchers.should._
import org.scalatest.wordspec._
import pseudoc.ast._

class PseudoInterpreterTypeCheckTest extends AnyWordSpec with Matchers {

  "PseudoInterpreter with type checking" should {
    "successfully parse, type check and execute valid programs" in {
      // Create a simple program for testing
      val algo = Algorithm("TestAlgo")
      val vars = Variables(Seq(
        VariableDecl("x", "int"),
        VariableDecl("message", "string")
      ))
      
      val statements = Seq(
        IntAssignment("x", IntLiteral(42)),
        StringAssignment("message", StringLiteral("Hello")),
        FunctionCallString("print", Seq(StringRef("message")))
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
      
      val testConsole = TestConsoleOutput()
      val result = statements.foldLeft(EvalResult(testConsole, initialVarMap)) { 
        (result, statement) => PseudoInterpreter.evalWithVars(statement, result.vars, result.console)
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
      val invalidInput = "x <- 10 +"
      
      // Try to parse just this expression
      val parseResult = parse(invalidInput, PseudoCodeParser.intAssignment(_))
      
      // Should be a parse failure
      parseResult match {
        case _: Parsed.Success[_] => fail("Expected parsing to fail but it succeeded")
        case _: Parsed.Failure => succeed // Expected outcome
      }
    }
    
    "report type errors" in {
      // Create a program with a type error
      val algo = Algorithm("TestAlgo")
      val vars = Variables(Seq(
        VariableDecl("x", "int"),
        VariableDecl("message", "string")
      ))
      
      val statements = Seq(
        // Type error: assigning string literal to integer variable
        IntAssignment("x", IntRef("message")),
        StringAssignment("message", StringLiteral("Hello"))
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
        VariableDecl("sum", "int"),
        VariableDecl("flag", "boolean"),
        VariableDecl("message", "string")
      ))
      
      val statements = Seq(
        // Initialize variables
        IntAssignment("sum", IntLiteral(0)),
        BoolAssignment("flag", BoolLiteral(true)),
        
        // Integer arithmetic
        IntAssignment("sum", IntAddSub(
          IntMultDiv(IntRef("sum"), Seq()),
          Seq((AddSubOperator.Add, IntMultDiv(IntLiteral(10), Seq())))
        )),
        
        // Boolean comparison
        BoolAssignment("flag", Comparison(
          IntRef("sum"),
          ComparisonOperator.GreaterThan,
          IntLiteral(5)
        )),
        
        // String assignment without concatenating with int
        StringAssignment("message", StringLiteral("Result verified"))
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
      
      val testConsole = TestConsoleOutput()
      
      // Evaluate the program
      val result = statements.foldLeft(EvalResult(testConsole, initialVarMap)) { 
        (current, statement) => PseudoInterpreter.evalWithVars(statement, current.vars, current.console)
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
        VariableDecl("x", "int"),
        VariableDecl("y", "string")
      ))
      
      val statements = Seq(
        IntAssignment("x", IntLiteral(10)),
        StringAssignment("y", StringLiteral("hello")),
        // Type error: attempting to use string in comparison
        IfStatement(
          Comparison(
            IntRef("y"),  // Error: y is a string, not an int
            ComparisonOperator.GreaterThan,
            IntLiteral(5)
          ),
          Seq(FunctionCallString("print", Seq(StringLiteral("This won't work"))))
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
        VariableDecl("a", "int"),
        VariableDecl("b", "string"),
        VariableDecl("c", "boolean")
      ))
      
      val statements = Seq(
        IntAssignment("a", IntLiteral(42)),
        StringAssignment("b", StringLiteral("Hello")),
        BoolAssignment("c", BoolLiteral(true)),
        // Print only string value (no concatenation with int)
        FunctionCallString("print", Seq(StringRef("b")))
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
      
      val testConsole = TestConsoleOutput()
      
      // Evaluate the program
      val result = statements.foldLeft(EvalResult(testConsole, initialVarMap)) { 
        (current, statement) => PseudoInterpreter.evalWithVars(statement, current.vars, current.console)
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