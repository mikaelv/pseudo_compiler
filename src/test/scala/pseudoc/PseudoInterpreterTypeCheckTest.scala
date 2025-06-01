package pseudoc

import org.scalatest._
import matchers.should._
import wordspec._
import pseudoc.ast._

class PseudoInterpreterTypeCheckTest extends AnyWordSpec with Matchers {

  "PseudoInterpreter with type checking" should {
    "successfully parse, type check and execute valid programs" in {
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
        |print(message)
      """.stripMargin
      
      val testConsole = TestConsoleOutput()
      val result = PseudoInterpreter.parseTypeCheckAndEval(source, testConsole)
      
      result shouldBe a[Right[_, _]]
      val evalResult = result.right.get
      
      // Check console output
      evalResult.console.getOutput should include("Valid: 11")
      
      // Check variables
      evalResult.vars.apply("x") shouldBe 11
      evalResult.vars.apply("isValid") shouldBe true
      evalResult.vars.apply("message") shouldBe "Valid: 11"
    }
    
    "report parse errors" in {
      val invalidSource = """
        |Algorithme: TestAlgo
        |Variables: x: int
        |x <- 10 +  // Syntax error: incomplete expression
      """.stripMargin
      
      val result = PseudoInterpreter.parseTypeCheckAndEval(invalidSource)
      
      result shouldBe a[Left[_, _]]
      result.left.get should include("Parse error")
    }
    
    "report type errors" in {
      val source = """
        |Algorithme: TestAlgo
        |Variables: x: int, message: string
        |x <- "not an integer"  // Type error
        |message <- "Hello"
      """.stripMargin
      
      val result = PseudoInterpreter.parseTypeCheckAndEval(source)
      
      result shouldBe a[Left[_, _]]
      result.left.get should include("Type error")
    }
    
    "handle complex programs with correct types" in {
      val source = """
        |Algorithme: ComplexTest
        |Variables: sum: int, i: int, msg: string, continue: boolean
        |sum <- 0
        |continue <- true
        |Pour i <- 1 a 10 Faire
        |  sum <- sum + i
        |  continue <- sum < 40
        |  Si continue Alors
        |    msg <- "Sum: " + sum
        |  Sinon
        |    msg <- "Exceeded limit"
        |  Fin Si
        |Fin Pour
        |print(msg)
      """.stripMargin
      
      val testConsole = TestConsoleOutput()
      val result = PseudoInterpreter.parseTypeCheckAndEval(source, testConsole)
      
      result shouldBe a[Right[_, _]]
      val evalResult = result.right.get
      
      // Check console output
      evalResult.console.getOutput should include("Exceeded limit")
      
      // Check variables
      evalResult.vars.apply("sum") shouldBe 55
      evalResult.vars.apply("continue") shouldBe false
    }
    
    "detect errors in conditional expressions" in {
      val source = """
        |Algorithme: ConditionTest
        |Variables: x: int, y: string
        |x <- 10
        |y <- "hello"
        |Si y > x Alors  // Error: comparing string with int
        |  print("This won't work")
        |Fin Si
      """.stripMargin
      
      val result = PseudoInterpreter.parseTypeCheckAndEval(source)
      
      result shouldBe a[Left[_, _]]
      result.left.get should include("Type error")
    }
    
    "validate variable declarations and initializations" in {
      val source = """
        |Algorithme: VarInitTest
        |Variables: a: int, b: string, c: boolean
        |a <- 42
        |b <- "Hello"
        |c <- true
        |print(b + a)
      """.stripMargin
      
      val testConsole = TestConsoleOutput()
      val result = PseudoInterpreter.parseTypeCheckAndEval(source, testConsole)
      
      result shouldBe a[Right[_, _]]
      val evalResult = result.right.get
      
      // Check console output
      evalResult.console.getOutput should include("Hello42")
      
      // Check variables
      evalResult.vars.apply("a") shouldBe 42
      evalResult.vars.apply("b") shouldBe "Hello"
      evalResult.vars.apply("c") shouldBe true
    }
  }
}