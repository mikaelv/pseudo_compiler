package pseudoc

import fastparse.*
import fastparse.JavaWhitespace.*
import pseudoc.BooleanExpressionParser.{boolExpr, boolFactor, comparisonExpr}
import pseudoc.IntExpressionParser.{intExpr, intLiteral}
import pseudoc.Lexical.{identifier, tpe, ws}
import pseudoc.StringExpressionParser.stringExpression
import pseudoc.ast.*

object PseudoCodeParser {

  // Context-aware variable reference parser
  def variableReference[$: P](implicit symbolTable: SymbolTable): P[Expression] = identifier.map {
    varName =>
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

  def forLoop[$: P](implicit symbols: SymbolTable): P[ForLoop] = P(
    StringIn("Pour", "For") ~ identifier ~ "<-" ~
      intLiteral ~ StringIn("à", "a", "to") ~ intLiteral ~ StringIn("Faire", "do") ~
      statement.rep ~ StringIn("Fin Pour", "fin pour", "End For", "end for")
  ).map(ForLoop.apply)


  def ifStatement[$: P](implicit symbols: SymbolTable): P[IfStatement] = P(
    StringIn("Si", "If") ~~ ws ~ boolFactor ~
      StringIn("Alors", "Then") ~~ ws ~ statement.rep ~
      (StringIn("Sinon", "Else") ~~ ws ~ statement.rep).? ~
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
    intExpr | stringExpression | boolExpr
  )

  // Context-aware statement parser
  def statement[$: P](implicit symbols: SymbolTable): P[Statement] =
    (forLoop | ifStatement | StringExpressionParser.print | assignment)

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
