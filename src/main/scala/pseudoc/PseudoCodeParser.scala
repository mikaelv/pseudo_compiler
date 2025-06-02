package pseudoc

import fastparse.*
import fastparse.JavaWhitespace.*
import pseudoc.BooleanExpressionParser.{boolFactor, comparisonExpr}
import pseudoc.IntExpressionParser.integer
import pseudoc.Lexical.{identifier, ws}
import pseudoc.ast.*

object PseudoCodeParser {

  // Context-aware variable reference parser
  def variableReference[$: P](implicit symbolTable: SymbolTable): P[Expression] = identifier.map { varName =>
    symbolTable.getType(varName) match {
      case Some(PseudoType.IntType)    => IntRef(varName)
      case Some(PseudoType.StringType) => StringRef(varName)
      case Some(PseudoType.BoolType)   => BoolRef(varName)
      // TODO how to report error ?
      case None => throw new RuntimeException(s"Undefined variable: $varName")
    }
  }

  def algo[$: P]: P[Algorithm] =
    P("Algorithme" ~ ":" ~ identifier).map(Algorithm.apply)

  /** "chaine de caracteres" must be before "chaine" */
  def typeString[$: P]: P[PseudoType] = P(
    StringIn(
      "chaine de caracteres",
      "chaine de caractères",
      "chaîne de caractères",
      "chaîne",
      "chaine",
      "string",
      "str"
    ).map(_ => PseudoType.StringType) |
      StringIn("int", "integer", "entier").map(_ => PseudoType.IntType) |
      StringIn("bool", "boolean", "booléen").map(_ => PseudoType.BoolType)
  )

  def tpe[$: P]: P[PseudoType] = typeString

  def variableDecl[$: P]: P[VariableDecl] =
    P(identifier ~ ":" ~ tpe).map(VariableDecl.apply)

  def variables[$: P]: P[Variables] = P(
    ("Variables" ~ ":") ~ variableDecl.rep(sep = ",")
  ).map(Variables.apply)

  // Helper function to build SymbolTable from Variables
  def buildSymbolTable(vars: Variables): SymbolTable = {
    vars.vars.foldLeft(SymbolTable()) { (table, varDecl) =>
      table.addVariable(varDecl.name, varDecl.tpe)
    }
  }

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

  def stringExpression[$: P]: P[StringConcat] =
    (identifier.map(StringRef.apply) | stringLiteral.map(StringLiteral.apply))
      .rep(sep = "+")
      .map(StringConcat.apply).log

  def print[$: P]: P[FunctionCallString] = P(
    StringIn(
      "Ecrire",
      "ecrire",
      "écrire",
      "Write",
      "write",
      "Print",
      "print"
    ) ~ "(" ~ stringExpression ~ ")"
  ).map(concat => FunctionCallString("print", Seq(concat)))

  def ifStatement[$: P](implicit symbols: SymbolTable): P[IfStatement] = P(
    StringIn("Si", "If") ~~ ws ~ boolFactor ~
      StringIn("Alors", "Then") ~~ ws ~ statementWithContext.rep ~
      (StringIn("Sinon", "Else") ~~ ws ~ statementWithContext.rep).? ~
      StringIn("Fin Si", "End If")
  ).map {
    case (condition, thenBranch, Some(elseBranch)) =>
      IfStatement(condition, thenBranch, Some(elseBranch))
    case (condition, thenBranch, None) =>
      IfStatement(condition, thenBranch, None)
  }.log

  // Context-aware assignment parser that resolves variable references using SymbolTable
  // TODO more coverage ?
  def assignmentWithContext[$: P](implicit symbols: SymbolTable): P[Assignment] = P(
    identifier ~ "<-" ~ expression
  ).map { case (variable, value) => Assignment(variable, value) }

  def expression[$: P](implicit symbols: SymbolTable): P[Expression] = (
    IntExpressionParser.addSub | stringExpression | BooleanExpressionParser.or
  )

  @deprecated("only for testing")
  def assignment[$: P]: P[Assignment] = assignmentWithContext(symbols = SymbolTable())

  // Context-aware statement parser
  def statementWithContext[$: P](implicit symbols: SymbolTable): P[Statement] =
    (forLoop | ifStatement | print | assignmentWithContext)

  @deprecated("only for testing")
  def statement[$: P]: P[Statement] = statementWithContext(symbols = SymbolTable()).log

  /** Parse a complete program consisting of algorithm, variables, and statements Uses context-aware
    * parsing to resolve variable references
    */
  def programWithContext[$: P]: P[Program] =
    for {
      algoResult <- algo
      varsResult <- variables
      _ <- StringIn("Début", "Debut", "debut", "Begin", "begin")
      symbolTable = buildSymbolTable(varsResult)
      statements <- statementWithContext(symbols = symbolTable).rep
      _ <- StringIn("Fin", "fin", "End", "end")
    } yield Program(algoResult, varsResult, statements)


  /** Parse input into a Program object using context-aware parsing
    */
  def parseProgram(input: String): Either[String, Program] = {
    import fastparse.*

    parse(input, programWithContext(_)) match {
      case Parsed.Success(program, _) => Right(program)
      case f: Parsed.Failure          => Left(s"${f.msg} (at index ${f.index})")
    }
  }

}
