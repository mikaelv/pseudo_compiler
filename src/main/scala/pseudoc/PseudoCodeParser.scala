package pseudoc

import fastparse.*
import fastparse.JavaWhitespace.*
import pseudoc.ArrayExpressionParser.arrayExpr
import pseudoc.BooleanExpressionParser.{boolExpr, boolFactor, comparisonExpr}
import pseudoc.IntExpressionParser.{intExpr, intLiteral}
import pseudoc.Lexical.{arrayIntType, identifier, lineFeed, spaceLF, tpe}
import pseudoc.StringExpressionParser.stringExpression
import pseudoc.ast.*

object PseudoCodeParser {

  // Context-aware variable reference parser
  def variableReference[$: P](implicit symbolTable: SymbolTable): P[Expression] = identifier.map {
    varName =>
      symbolTable.getType(varName) match {
        case Some(PseudoType.IntType)     => IntRef(varName)
        case Some(PseudoType.StringType)  => StringRef(varName)
        case Some(PseudoType.BoolType)    => BoolRef(varName)
        case Some(PseudoType.ArrayIntType) => ArrayRef(varName)
        // TODO how to report error ?
        case None => throw new RuntimeException(s"Undefined variable: $varName")
      }
  }

  def algo[$: P]: P[Algorithm] =
    P("Algorithme" ~ ":" ~ identifier).map(Algorithm.apply)

  /** syntax: ```s0, s1, s2: string``` */
  def regularVariableDecl[$: P]: P[Variables] =
    P(identifier.rep(1, sep=",") ~ ":" ~ tpe).map((varNames, tpe) =>
      Variables(varNames.map(varName => VariableDecl(varName, tpe))))

  /** syntax: ```arr [10] : tableau d'entier``` */
  def arrayVariableDecl[$: P]: P[Variables] =
    P(identifier ~ "[" ~ intLiteral ~ "]" ~ ":" ~ arrayIntType).map { case (varName, size, arrayType) =>
      Variables(Seq(ArrayVariableDecl(varName, arrayType, size.value)))
    }

  def variableDecl[$: P]: P[Variables] = P(arrayVariableDecl | regularVariableDecl).log

  /** One line per type */
  def variables[$: P]: P[Variables] = P(
    // repX to prevent \n being consumed by JavaWhiteSpace
    ("Variables" ~ ":") ~ variableDecl.repX(sep=lineFeed)
  ).map(Variables.fromSeq)

  // Helper function to build SymbolTable from Variables
  def buildSymbolTable(vars: Variables): SymbolTable = {
    vars.vars.foldLeft(SymbolTable()) { (table, varDecl) =>
      table.addVariable(varDecl.name, varDecl.tpe)
    }
  }

  def forLoop[$: P](implicit symbols: SymbolTable): P[ForLoop] = P(
    StringIn("Pour", "For") ~ identifier ~ "<-" ~
      intLiteral ~ StringIn("à", "a", "to") ~ intLiteral ~ StringIn("Faire", "do") ~
      statement.rep ~ StringIn("Fin Pour", "fin pour", "End For", "end for")
  ).map(ForLoop.apply)

  def whileLoop[$: P](implicit symbols: SymbolTable): P[WhileLoop] = P(
    StringIn("Tant que", "While") ~ boolFactor ~ StringIn("Faire", "do") ~
      statement.rep ~ StringIn("Fin Tant que", "fin tant que", "End While", "end while")
  ).map(WhileLoop.apply)


  def ifStatement[$: P](implicit symbols: SymbolTable): P[IfStatement] = P(
    StringIn("Si", "If") ~~ spaceLF ~ boolFactor ~
      StringIn("Alors", "Then") ~~ spaceLF ~ statement.rep ~
      (StringIn("Sinon", "Else") ~~ spaceLF ~ statement.rep).? ~
      StringIn("Fin Si", "End If")
  ).map {
    case (condition, thenBranch, Some(elseBranch)) =>
      IfStatement(condition, thenBranch, Some(elseBranch))
    case (condition, thenBranch, None) =>
      IfStatement(condition, thenBranch, None)
  }.log

  // Context-aware assignment parser that resolves variable references using SymbolTable
  def assignment[$: P](implicit symbols: SymbolTable): P[Assignment] = P(
    identifier ~ "<-" ~ expression
  ).map { case (variable, value) => Assignment(variable, value) }

  def expression[$: P](implicit symbols: SymbolTable): P[Expression] = (
    intExpr | arrayExpr | stringExpression | boolExpr
  )

  // Context-aware statement parser
  def statement[$: P](implicit symbols: SymbolTable): P[Statement] = P(
    (forLoop | whileLoop | ifStatement | StringExpressionParser.print | StringExpressionParser.read | assignment))

  /** Parse a complete program consisting of algorithm, variables, and statements Uses context-aware
    * parsing to resolve variable references
    */
  def program[$: P]: P[Program] =
    for {
      algoResult <- algo
      varsResult <- variables
      _ <- StringIn("Début", "Debut", "debut", "Begin", "begin")
      symbolTable = buildSymbolTable(varsResult)
      statements <- statement(symbols = symbolTable).rep
      _ <- StringIn("Fin", "fin", "End", "end")
    } yield Program(algoResult, varsResult, statements)

  /** Parse input into a Program object using context-aware parsing
    */
  def parseProgram(input: String): Either[String, Program] = {
    import fastparse.*

    parse(input, program(_)) match {
      case Parsed.Success(program, _) => Right(program)
      case f: Parsed.Failure          => Left(s"${f.msg} (at index ${f.index})")
    }
  }

}
