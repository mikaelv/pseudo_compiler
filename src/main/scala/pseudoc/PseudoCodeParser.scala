package pseudoc

import fastparse.*
import JavaWhitespace.*
import pseudoc.BooleanExpressionParser.{booleanExpression, comparisonExpr}
import pseudoc.IntExpressionParser.integer
import pseudoc.ast.*
import pseudoc.{SymbolTable, PseudoType}


object PseudoCodeParser {

  def identifier[$: P]: P[String] =
    (CharIn("a-zA-Z") ~~ CharIn("a-zA-Z_0-9").rep).!

  // Context-aware variable reference parser
  def variableReference[$: P](symbolTable: SymbolTable): P[Expression] = identifier.map { varName =>
    symbolTable.getType(varName) match {
      case Some(PseudoType.IntType) => IntRef(varName)
      case Some(PseudoType.StringType) => StringRef(varName)
      case Some(PseudoType.BoolType) => BoolRef(varName)
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

  def tpe[$: P] = typeString

  def variableDecl[$: P]: P[VariableDecl] =
    P(identifier ~ ":" ~ tpe).map(VariableDecl.apply)

  def variables[$: P]: P[Variables] = P(
    "Variables" ~ ":" ~ variableDecl.rep(sep = ",")
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
    StringIn("Si", "If") ~ comparisonExpr ~
      StringIn("Alors", "Then") ~ statement.rep ~
      (StringIn("Sinon", "Else") ~ statement.rep).? ~
      StringIn("Fin Si", "End If")
  ).map {
    case (condition, thenBranch, Some(elseBranch)) =>
      IfStatement(condition, thenBranch, Some(elseBranch))
    case (condition, thenBranch, None) =>
      IfStatement(condition, thenBranch, None)
  }
  
  // Context-aware assignment parser that resolves variable references using SymbolTable
  def assignmentWithContext[$: P](symbolTable: SymbolTable): P[Assignment] = P(
    identifier ~ "<-" ~ variableReference(symbolTable)
  ).map { case (variable, value) => Assignment(variable, value) }

  // Assignment parser - tries different expression types in order  
  def assignment[$: P]: P[Assignment] = P(
    identifier ~ "<-" ~ (
      IntExpressionParser.addSub.map(_.asInstanceOf[Expression]) |
      expressionString.map(_.asInstanceOf[Expression]) |
      BooleanExpressionParser.or.map(_.asInstanceOf[Expression])
    )
  ).map { case (variable, value) => Assignment(variable, value) }

  // Context-aware statement parser
  def statementWithContext[$: P](symbolTable: SymbolTable): P[Statement] = 
    (forLoop | ifStatement | print | assignmentWithContext(symbolTable))

  def statement[$: P]: P[Statement] = (forLoop | ifStatement | print | assignment)
  
  /**
   * Parse a complete program consisting of algorithm, variables, and statements
   * Uses context-aware parsing to resolve variable references
   */
  def programWithContext[$: P]: P[Program] = 
    for {
      algoResult <- algo
      varsResult <- variables
      symbolTable = buildSymbolTable(varsResult)
      statements <- statementWithContext(symbolTable).rep
    } yield Program(algoResult, varsResult, statements)

  /**
   * Parse a complete program consisting of algorithm, variables, and statements
   */
  def program[$: P]: P[Program] = 
    P(algo ~ variables ~ statement.rep).map { 
      case (algo, vars, statements) => Program(algo, vars, statements)
    }
    
  /**
   * Parse input into a Program object using context-aware parsing
   */
  def parseProgram(input: String): Either[String, Program] = {
    import fastparse._
    
    parse(input, programWithContext(_)) match {
      case Parsed.Success(program, _) => Right(program)
      case f: Parsed.Failure => Left(s"${f.msg} (at index ${f.index})")
    }
  }

}