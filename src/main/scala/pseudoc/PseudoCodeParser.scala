package pseudoc

import fastparse.*
import JavaWhitespace.*
import pseudoc.Ast.{Algorithm, ForLoop, Instruction, VariableDecl, Variables}

object PseudoCodeParser {
  /*
Algorithme : recherche_dichotomique
Variables :
l, r, m : entier
iter, pos, val : entier
T [10] : tableau d’entier
Début
1: T ← {1, 1, 2, 3, 5, 8, 13, 21, 34, 55}
2: Lire(val)
3: pos ← −1
4: iter ← 0
5: l ← 1
6: r ← 10
7: Tant que l ≤ r ET pos = −1 Faire
8: iter ← iter + 1
9: m ← l + (r − l)/2
10: Si T [m] = val Alors
11: pos ← m
12: Sinon
13: Si T [m] < val Alors
14: l ← m + 1
15: Sinon
16: r ← m − 1
17: Fin Si
18: Fin Si
19: Fin Tant que
20: Si pos̸ = −1 Alors
21: Ecrire("Valeur trouvée en ", iter, "itérations\NL")
22: Fin Si
Fin
   */
  def identifier[$: P]: P[String] =
    (CharIn("a-zA-Z") ~ CharIn("a-zA-Z_0-9").rep).!

  def algo[$: P]: P[Algorithm] =
    P("Algorithme" ~ ":" ~ identifier).map(Algorithm.apply)

  /** "chaine de caracteres" must be before "chaine" */
  def typeString[$: P]: P[String] = P(
    StringIn("chaine de caracteres", "chaine de caractères", "chaîne de caractères", "chaîne", "chaine", "string", "str")
  ).map(_ => "string")

  def tpe[$: P] = typeString

  def variableDecl[$: P]: P[VariableDecl] =
    P(identifier ~ ":" ~ tpe).map(VariableDecl.apply)

  def variables[$: P]: P[Variables] = P(
    "Variables" ~ ":" ~ variableDecl.rep(sep = ",")
  ).map(Variables.apply)

  def digits[$: P]: P[Unit] = P( CharsWhileIn("0-9") )
  def integer[$: P]: P[Int] = digits.!.map(_.toInt)

  def forLoop[$: P]  = P(
    StringIn("Pour","For") ~ identifier ~ "<-" ~
      integer ~ StringIn("à", "a", "to") ~ integer ~ StringIn("Faire", "do") ~
      instruction.rep ~ StringIn("Fin Pour", "fin pour", "End For", "end for")
  ).map(ForLoop.apply)

  def instruction[$: P]: P[Instruction] = forLoop

}
