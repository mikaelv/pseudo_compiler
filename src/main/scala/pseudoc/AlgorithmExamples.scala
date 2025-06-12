package pseudoc

case class AlgorithmExample(name: String, code: String, description: String)

object AlgorithmExamples {
  
  val examples = Map(
    "hello_world" -> AlgorithmExample(
      "Hello World",
      """Algorithme: hello_world
Variables:
  x : entier
  message : chaine

Debut
  x <- 25
  message <- "Hello World "
  Ecrire(message, x, "\NL")
Fin""",
      "Simple example showing variable assignment and output"
    ),
    
    "factorial" -> AlgorithmExample(
      "Factorial Calculator",
      """Algorithme : factorial
Variables :
    n, result, i : entier
Début
    Lire(n)
    result <- 1
    i <- 1
    Tant que i <= n Faire
        result <- result * i
        i <- i + 1
    Fin Tant que
    Ecrire("Factorial of ", n, " is ", result, "\NL")
Fin""",
      "Calculate factorial using iterative approach"
    ),
    
    "binary_search" -> AlgorithmExample(
      "Binary Search",
      """Algorithme : recherche_dichotomique
Variables :
    l, r, m : entier
    iter, pos, val : entier
    T [10] : tableau d'entier
Début
    T <- {1, 1, 2, 3, 5, 8, 13, 21, 34, 55}
    Lire(val)
    pos <- -1
    iter <- 0
    l <- 1
    r <- 10
    Tant que l <= r ET pos = -1 Faire
        iter <- iter + 1
        m <- l + (r - l)/2
        Si T[m] = val Alors
            pos <- m
        Sinon
            Si T[m] < val Alors
                l <- m + 1
            Sinon
                r <- m - 1
            Fin Si
        Fin Si
    Fin Tant que
    Si pos != -1 Alors
        Ecrire("Valeur trouvée en ", iter, " itérations à la position ", pos, "\NL")
    Sinon
        Ecrire("Valeur non trouvée après ", iter, " itérations\NL")
    Fin Si
Fin""",
      "Search for a value in a sorted array using binary search"
    ),
    
    "array_sum" -> AlgorithmExample(
      "Array Sum",
      """Algorithme : sum_array
Variables :
    arr [5] : tableau d'entier
    sum, i : entier
Début
    arr <- {10, 20, 30, 40, 50}
    sum <- 0
    i <- 1
    Tant que i <= 5 Faire
        sum <- sum + arr[i]
        i <- i + 1
    Fin Tant que
    Ecrire("Sum of array elements: ", sum, "\NL")
Fin""",
      "Calculate the sum of all elements in an array"
    )
  )
}