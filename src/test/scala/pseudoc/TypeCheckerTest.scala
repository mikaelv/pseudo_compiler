package pseudoc

import org.scalatest._
import matchers.should._
import wordspec._
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
        StringAssignment("stringVar", StringLiteral("hello")),
        IntAssignment("intVar", IntLiteral(42)),
        BoolAssignment("boolVar", BoolLiteral(true)),
        
        // Correct variable references
        StringAssignment("stringVar", StringRef("stringVar")),
        IntAssignment("intVar", IntRef("intVar")),
        BoolAssignment("boolVar", BoolRef("boolVar")),
        
        // Correct expressions
        IntAssignment("intVar", IntAddSub(
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
        StringAssignment("knownVar", StringRef("unknownVar"))
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
        IntAssignment("intVar", IntRef("stringVar"))
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
        IntAssignment("intVar", IntAddSub(
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
    
    "validate complete programs" in {
      // Test a small complete program
      val source = """
        |Algorithme: TestAlgo
        |Variables: x: int, message: string, isValid: boolean
        |x <- 10
        |message <- "Count: " + x
        |isValid <- x > 5
        |Si isValid Alors
        |  x <- x + 1
        |  message <- "Valid: " + x
        |Fin Si
        |Pour i <- 1 a 5 Faire
        |  x <- x + i
        |Fin Pour
        |print(message)
      """.stripMargin
      
      val program = PseudoCodeParser.parseProgram(source)
      program shouldBe a[Right[_, _]]
      
      val typeCheckResult = program.right.get.typeCheck()
      typeCheckResult shouldBe a[Right[_, _]]
    }
    
    "detect errors in complete programs" in {
      // Program with type error
      val source = """
        |Algorithme: TestAlgo
        |Variables: x: int, message: string, isValid: boolean
        |x <- 10
        |message <- "Count: " + x
        |isValid <- message  // Error: assigning string to boolean
        |print(message)
      """.stripMargin
      
      val program = PseudoCodeParser.parseProgram(source)
      program shouldBe a[Right[_, _]]
      
      val typeCheckResult = program.right.get.typeCheck()
      typeCheckResult shouldBe a[Left[_, _]]
      typeCheckResult.left.get should include("Type mismatch")
    }
  }
}