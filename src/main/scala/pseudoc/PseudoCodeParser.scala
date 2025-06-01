package pseudoc

import fastparse.*
import JavaWhitespace.*
import pseudoc.BooleanExpressionParser.booleanExpression
import pseudoc.IntExpressionParser.integer
import pseudoc.ast.*


object PseudoCodeParser {

  def identifier[$: P]: P[String] =
    (CharIn("a-zA-Z") ~~ CharIn("a-zA-Z_0-9").rep).!

  def algo[$: P]: P[Algorithm] =
    P("Algorithme" ~ ":" ~ identifier).map(Algorithm.apply)

  /** "chaine de caracteres" must be before "chaine" */
  def typeString[$: P]: P[String] = P(
    StringIn(
      "chaine de caracteres",
      "chaine de caractères",
      "chaîne de caractères",
      "chaîne",
      "chaine",
      "string",
      "str"
    )
  ).map(_ => "string")

  def tpe[$: P] = typeString

  def variableDecl[$: P]: P[VariableDecl] =
    P(identifier ~ ":" ~ tpe).map(VariableDecl.apply)

  def variables[$: P]: P[Variables] = P(
    "Variables" ~ ":" ~ variableDecl.rep(sep = ",")
  ).map(Variables.apply)


  def forLoop[$: P] = P(
    StringIn("Pour", "For") ~ identifier ~ "<-" ~
      integer ~ StringIn("à", "a", "to") ~ integer ~ StringIn("Faire", "do") ~
      statement.rep ~ StringIn("Fin Pour", "fin pour", "End For", "end for")
  ).map(ForLoop.apply)

  def stringChars(c: Char) = c != '\"'
  def strChars[$: P] = P(CharsWhile(stringChars))
  // TODO there might be a way to handle escape chars more gracefully
  def stringLiteral[$: P]: P[String] = P(
    "\"" ~/ (strChars).rep.! ~ "\""
  ).map(_.replaceAll("\\\\NL", "\n"))

  def expressionString[$: P]: P[StringConcat] =
    (identifier.map(StringRef.apply) | stringLiteral.map(StringLiteral.apply))
      .rep(sep = "+")
      .map(StringConcat.apply)

  def print[$: P]: P[FunctionCallString] = P(
    StringIn(
      "Ecrire",
      "ecrire",
      "écrire",
      "Write",
      "write",
      "Print",
      "print"
    ) ~ "(" ~ expressionString ~ ")"
  ).map(concat => FunctionCallString("print", Seq(concat)))



  def ifStatement[$: P]: P[IfStatement] = P(
    StringIn("Si", "If") ~/ booleanExpression ~
      StringIn("Alors", "Then") ~/ statement.rep ~
      (StringIn("Sinon", "Else") ~/ statement.rep).? ~
      StringIn("Fin Si", "End If")
  ).map {
    case (condition, thenBranch, Some(elseBranch)) =>
      IfStatement(condition, thenBranch, Some(elseBranch))
    case (condition, thenBranch, None) =>
      IfStatement(condition, thenBranch, None)
  }
  
  // Integer assignment - this should come first in the assignment parser
  def intAssignment[$: P]: P[IntAssignment] = P(
    identifier ~ "<-" ~ IntExpressionParser.addSub
  ).map { case (variable, value) => IntAssignment(variable, value) }
  
  // String assignment
  def stringAssignment[$: P]: P[StringAssignment] = P(
    identifier ~ "<-" ~ expressionString
  ).map { case (variable, value) => StringAssignment(variable, value) }

  def boolAssignment[$: P]: P[BoolAssignment] = P(
    identifier ~ "<-" ~ BooleanExpressionParser.or
  ).map { case (variable, value) => BoolAssignment(variable, value) }


  // Combined assignment - try int assignment first to avoid ambiguity
  def assignment[$: P]: P[Assignment] = (boolAssignment | intAssignment | stringAssignment).log

  def statement[$: P]: P[Statement] = (forLoop | ifStatement | print | assignment).log

}