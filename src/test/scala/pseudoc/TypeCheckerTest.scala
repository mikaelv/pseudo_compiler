package pseudoc

import org.scalatest._
import org.scalatest.matchers.should._
import org.scalatest.wordspec._
import pseudoc.ast._

class TypeCheckerTest extends AnyWordSpec with Matchers {

  "TypeChecker" should {
    "accept correct variable usages" in {
      // Create a program with correct variable usage
      val algo = Algorithm("Test")
      val vars = Variables(Seq(
        VariableDecl("stringVar", "string"),
        VariableDecl("intVar", "int"),
        VariableDecl("boolVar", "boolean")
      ))
      
      val statements = Seq(
        // Correct assignments
        Assignment("stringVar", StringLiteral("hello")),
        Assignment("intVar", IntLiteral(42)),
        Assignment("boolVar", BoolLiteral(true)),
        
        // Correct variable references
        Assignment("stringVar", StringRef("stringVar")),
        Assignment("intVar", IntRef("intVar")),
        Assignment("boolVar", BoolRef("boolVar")),
        
        // Correct expressions
        Assignment("intVar", IntAddSub(
          IntMultDiv(IntLiteral(5), Seq((MultDivOperator.Mult, IntRef("intVar")))),
          Seq((AddSubOperator.Add, IntMultDiv(IntLiteral(2), Seq())))
        )),
        
        // Correct print statements
        FunctionCallString("print", Seq(StringLiteral("Hello")))
      )
      
      val program = Program(algo, vars, statements)
      
      // Should not produce type errors
      program.typeCheck() shouldBe a[Right[_, _]]
    }
    
    "detect undefined variables" in {
      val algo = Algorithm("Test")
      val vars = Variables(Seq(
        VariableDecl("knownVar", "string")
      ))
      
      val statements = Seq(
        // Reference to undefined variable
        Assignment("knownVar", StringRef("unknownVar"))
      )
      
      val program = Program(algo, vars, statements)
      
      // Should detect undefined variable
      val result = program.typeCheck()
      result shouldBe a[Left[_, _]]
      result.left.get should include("Undefined variable")
      result.left.get should include("unknownVar")
    }
    
    "detect type mismatches in assignments" in {
      val algo = Algorithm("Test")
      val vars = Variables(Seq(
        VariableDecl("stringVar", "string"),
        VariableDecl("intVar", "int"),
        VariableDecl("boolVar", "boolean")
      ))
      
      val statements = Seq(
        // Type mismatch: assigning string to int variable
        Assignment("intVar", IntRef("stringVar"))
      )
      
      val program = Program(algo, vars, statements)
      
      // Should detect type mismatch
      val result = program.typeCheck()
      result shouldBe a[Left[_, _]]
      result.left.get should include("Type mismatch")
    }
    
    "detect type mismatches in expressions" in {
      val algo = Algorithm("Test")
      val vars = Variables(Seq(
        VariableDecl("stringVar", "string"),
        VariableDecl("intVar", "int")
      ))
      
      val statements = Seq(
        // Using string variable in integer expression
        Assignment("intVar", IntAddSub(
          IntMultDiv(IntLiteral(5), Seq()),
          Seq((AddSubOperator.Add, IntMultDiv(IntRef("stringVar"), Seq())))
        ))
      )
      
      val program = Program(algo, vars, statements)
      
      // Should detect type mismatch
      val result = program.typeCheck()
      result shouldBe a[Left[_, _]]
      result.left.get should include("Type mismatch")
    }
    
    "detect string-int concatenation errors" in {
      // Create a program with string-int concatenation
      val algo = Algorithm("TestAlgo")
      val vars = Variables(Seq(
        VariableDecl("x", "int"),
        VariableDecl("message", "string")
      ))
      
      val statements = Seq(
        Assignment("x", IntLiteral(10)),
        // Type error: attempting to concatenate string with int
        Assignment("message", StringConcat(Seq(
          StringLiteral("Count: "), 
          StringRef("x")  // x is an int, not a string
        )))
      )
      
      val program = Program(algo, vars, statements)
      
      // Type checking should fail
      val result = program.typeCheck()
      result shouldBe a[Left[_, _]]
      result.left.get should include("Type mismatch")
    }
    
    "validate complete programs" in {
      // Create a program directly instead of parsing
      val algo = Algorithm("TestAlgo")
      val vars = Variables(Seq(
        VariableDecl("x", "int"),
        VariableDecl("message", "string"),
        VariableDecl("isValid", "boolean")
      ))
      
      val statements = Seq(
        Assignment("x", IntLiteral(10)),
        // Using only string literal - no direct concatenation with int
        Assignment("message", StringLiteral("Count: 10")),
        Assignment("isValid", Comparison(
          IntRef("x"), 
          ComparisonOperator.GreaterThan, 
          IntLiteral(5)
        ))
      )
      
      val program = Program(algo, vars, statements)
      
      // Type checking should pass
      program.typeCheck() shouldBe a[Right[_, _]]
    }
    
    "detect errors in complete programs" in {
      // Create a program directly with a type error
      val algo = Algorithm("TestAlgo")
      val vars = Variables(Seq(
        VariableDecl("x", "int"),
        VariableDecl("message", "string"),
        VariableDecl("isValid", "boolean")
      ))
      
      val statements = Seq(
        Assignment("x", IntLiteral(10)),
        // Using only string literal - no direct concatenation with int
        Assignment("message", StringLiteral("Count: 10")),
        // Type error: assigning string reference to boolean variable
        Assignment("isValid", BoolRef("message"))
      )
      
      val program = Program(algo, vars, statements)
      
      // Type checking should fail
      val typeCheckResult = program.typeCheck()
      typeCheckResult shouldBe a[Left[_, _]]
      typeCheckResult.left.get should include("Type mismatch")
    }
  }
}