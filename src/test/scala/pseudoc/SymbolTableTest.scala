package pseudoc

import org.scalatest._
import org.scalatest.matchers.should._
import org.scalatest.wordspec._
import pseudoc.PseudoType

class SymbolTableTest extends AnyWordSpec with Matchers {

  "SymbolTable" should {
    "store and retrieve variable types" in {
      val symbolTable = SymbolTable()
        .addVariable("strVar", classOf[String])
        .addVariable("intVar", classOf[Int])
        .addVariable("boolVar", classOf[Boolean])
      
      symbolTable.getType("strVar") shouldBe Some(PseudoType.StringType)
      symbolTable.getType("intVar") shouldBe Some(PseudoType.IntType)
      symbolTable.getType("boolVar") shouldBe Some(PseudoType.BoolType)
      symbolTable.getType("unknownVar") shouldBe None
    }
    
    "check if variables exist with expected types" in {
      val symbolTable = SymbolTable()
        .addVariable("strVar", classOf[String])
        .addVariable("intVar", classOf[Int])
      
      symbolTable.checkType("strVar", PseudoType.StringType) shouldBe true
      symbolTable.checkType("intVar", PseudoType.IntType) shouldBe true
      symbolTable.checkType("strVar", PseudoType.IntType) shouldBe false
      symbolTable.checkType("intVar", PseudoType.StringType) shouldBe false
      symbolTable.checkType("unknownVar", PseudoType.StringType) shouldBe false
    }
    
    "update variable types" in {
      val symbolTable = SymbolTable()
        .addVariable("testVar", classOf[String])
      
      symbolTable.getType("testVar") shouldBe Some(PseudoType.StringType)
      
      val updatedTable = symbolTable.addVariable("testVar", classOf[Int])
      updatedTable.getType("testVar") shouldBe Some(PseudoType.IntType)
      
      // Original table should be unchanged (immutability)
      symbolTable.getType("testVar") shouldBe Some(PseudoType.StringType)
    }
    
    "verify variable types with detailed error messages" in {
      val symbolTable = SymbolTable()
        .addVariable("strVar", classOf[String])
        .addVariable("intVar", classOf[Int])
      
      // Successful check
      symbolTable.checkVariableForType("strVar", PseudoType.StringType) shouldBe Right(PseudoType.StringType)
      
      // Type mismatch
      val mismatchResult = symbolTable.checkVariableForType("strVar", PseudoType.IntType)
      mismatchResult shouldBe a[Left[_, _]]
      mismatchResult.left.get should include("Type mismatch")
      mismatchResult.left.get should include("string")
      mismatchResult.left.get should include("int")
      
      // Undefined variable
      val undefinedResult = symbolTable.checkVariableForType("unknownVar", PseudoType.StringType)
      undefinedResult shouldBe a[Left[_, _]]
      undefinedResult.left.get should include("Undefined variable")
    }
    
    "be immutable" in {
      val symbolTable1 = SymbolTable()
      val symbolTable2 = symbolTable1.addVariable("var1", classOf[String])
      val symbolTable3 = symbolTable2.addVariable("var2", classOf[Int])
      
      // Each operation should create a new instance
      symbolTable1.types.size shouldBe 0
      symbolTable2.types.size shouldBe 1
      symbolTable3.types.size shouldBe 2
      
      // Original tables should be unchanged
      symbolTable1.getType("var1") shouldBe None
      symbolTable2.getType("var2") shouldBe None
      
      // Only the latest table should have all variables
      symbolTable3.getType("var1") shouldBe Some(PseudoType.StringType)
      symbolTable3.getType("var2") shouldBe Some(PseudoType.IntType)
    }
  }
}