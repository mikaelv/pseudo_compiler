package pseudoc

import org.scalatest.EitherValues.*
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.wordspec.AnyWordSpec

class PseudoCodeExamplesTest extends AnyWordSpec {
  def run(code: String, input: String = ""): EvalResult = {
    PseudoInterpreter.run(code, TestConsoleIO(input = input)).value
  }

  // Shared binary search algorithm code
  private val binarySearchCode = 
    """Algorithme : recherche_dichotomique
      |Variables :
      |    l, r, m : entier
      |    iter, pos, val : entier
      |    T [10] : tableau d'entier
      |Début
      |    T <- {1, 1, 2, 3, 5, 8, 13, 21, 34, 55}
      |    Lire(val)
      |    pos <- -1
      |    iter <- 0
      |    l <- 1
      |    r <- 10
      |    Tant que l <= r ET pos = -1 Faire
      |        iter <- iter + 1
      |        m <- l + (r - l)/2
      |        Si T[m] = val Alors
      |            pos <- m
      |        Sinon
      |            Si T[m] < val Alors
      |                l <- m + 1
      |            Sinon
      |                r <- m - 1
      |            Fin Si
      |        Fin Si
      |    Fin Tant que
      |    Si pos != -1 Alors
      |        Ecrire("Valeur trouvée en ", iter, " itérations à la position ", pos, "\NL")
      |    Sinon
      |        Ecrire("Valeur non trouvée après ", iter, " itérations\NL")
      |    Fin Si
      |Fin
      |""".stripMargin

  // Helper method for binary search tests
  private def testBinarySearch(
    testName: String, 
    searchValue: String, 
    expectedPos: Int, 
    expectedIter: Int,
    expectedOutput: String
  ): Unit = {
    testName in {
      val res = run(binarySearchCode, s"$searchValue\n")
      res.vars("pos") shouldBe expectedPos
      res.vars("iter") shouldBe expectedIter
      res.console.getOutput shouldBe expectedOutput
    }
  }

  "binary search" should {
    testBinarySearch(
      "find value in array", 
      "5", 
      5, 1, 
      "Valeur trouvée en 1 itérations à la position 5\n"
    )
    
    testBinarySearch(
      "not find value in array", 
      "100", 
      -1, 4, 
      "Valeur non trouvée après 4 itérations\n"
    )
    
    testBinarySearch(
      "find last element", 
      "55", 
      10, 4, 
      "Valeur trouvée en 4 itérations à la position 10\n"
    )
    
    testBinarySearch(
      "find first element", 
      "1", 
      2, 2, 
      "Valeur trouvée en 2 itérations à la position 2\n"
    )

    testBinarySearch(
      "find middle-right element",
      "21",
      8, 2,
      "Valeur trouvée en 2 itérations à la position 8\n"
    )
  }

  // Shared factorial algorithm code
  private val factorialCode = 
    """Algorithme : factorial
      |Variables :
      |    n, result, i : entier
      |Début
      |    Lire(n)
      |    result <- 1
      |    i <- 1
      |    Tant que i <= n Faire
      |        result <- result * i
      |        i <- i + 1
      |    Fin Tant que
      |    Ecrire("Factorial of ", n, " is ", result, "\NL")
      |Fin
      |""".stripMargin

  // Helper method for factorial tests
  private def testFactorial(
    testName: String,
    input: String,
    expectedResult: Int,
    expectedN: Int,
    expectedI: Int,
    expectedOutput: String
  ): Unit = {
    testName in {
      val res = run(factorialCode, s"$input\n")
      res.vars("result") shouldBe expectedResult
      res.vars("n") shouldBe expectedN
      res.vars("i") shouldBe expectedI
      res.console.getOutput shouldBe expectedOutput
    }
  }

  "factorial calculation" should {
    testFactorial(
      "calculate factorial iteratively",
      "5",
      120, 5, 6, // 5! = 120, loop variable after completion
      "Factorial of 5 is 120\n"
    )

    testFactorial(
      "handle factorial of 0",
      "0", 
      1, 0, 1, // 0! = 1, loop never executed
      "Factorial of 0 is 1\n"
    )

    testFactorial(
      "calculate factorial of 3",
      "3",
      6, 3, 4, // 3! = 6, loop variable after completion  
      "Factorial of 3 is 6\n"
    )
  }

  "sum of array elements" should {
    "calculate sum using while loop" in {
      val code: String =
        """Algorithme : sum_array
          |Variables :
          |    arr [5] : tableau d'entier
          |    sum, i : entier
          |Début
          |    arr <- {10, 20, 30, 40, 50}
          |    sum <- 0
          |    i <- 1
          |    Tant que i <= 5 Faire
          |        sum <- sum + arr[i]
          |        i <- i + 1
          |    Fin Tant que
          |    Ecrire("Sum of array elements: ", sum, "\NL")
          |Fin
          |""".stripMargin

      val res = run(code)
      res.vars("sum") shouldBe (150) // 10+20+30+40+50 = 150
      res.vars("i") shouldBe (6) // Loop variable after completion
      res.console.getOutput shouldBe "Sum of array elements: 150\n"
    }
  }

}
