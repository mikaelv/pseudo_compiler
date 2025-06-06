package pseudoc

import org.scalatest.EitherValues.*
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.wordspec.AnyWordSpec

class PseudoCodeExamplesTest extends AnyWordSpec {
  def run(code: String, input: String = ""): EvalResult = {
    PseudoInterpreter.run(code, TestConsoleIO(input)).value
  }

  "binary search" should {
    "french" in {
      val code: String =
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
          |    Si pos = -1 Alors
          |        Ecrire("Valeur trouvée en ", iter, "itérations\NL")
          |    Fin Si
          |Fin
          |""".stripMargin

      val res = run(code, "5\n")
      res.vars("pos") shouldBe (5)

    }

  }

}
