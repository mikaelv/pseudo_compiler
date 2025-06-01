package pseudoc.ast

import pseudoc.SymbolTable

sealed trait BoolExpression extends TypedExpression[Boolean] {
  override def expressionType: Class[Boolean] = classOf[Boolean]
}

case class Comparison(
    left: IntExpression,
    op: ComparisonOperator,
    right: IntExpression
) extends BoolExpression {
  override def typeCheck(symbolTable: SymbolTable): Either[String, Unit] = {
    // Check both sides of the comparison
    val leftResult = left.typeCheck(symbolTable)
    val rightResult = right.typeCheck(symbolTable)
    
    val errors = Seq(leftResult, rightResult).collect { case Left(error) => error }
    if (errors.isEmpty) Right(()) else Left(errors.mkString("\n"))
  }
}

enum ComparisonOperator:
  case Equal, NotEqual, LessThan, GreaterThan, LessThanEqual, GreaterThanEqual

case class BoolRef(varName: String) extends BoolExpression {
  override def typeCheck(symbolTable: SymbolTable): Either[String, Unit] = {
    symbolTable.checkVariableForType(varName, classOf[Boolean]).map(_ => ())
  }
}

case class BoolLiteral(value: Boolean) extends BoolExpression {
  // Boolean literals are always type-correct
  override def typeCheck(symbolTable: SymbolTable): Either[String, Unit] = Right(())
}

enum BooleanOperator:
  case And, Or

case class BoolOperations(base: BoolExpression, ops: (BooleanOperator, BoolExpression)*)
    extends BoolExpression {
  override def typeCheck(symbolTable: SymbolTable): Either[String, Unit] = {
    // Check base expression
    val baseResult = base.typeCheck(symbolTable)
    
    // Check all operands
    val opResults = ops.map(_._2.typeCheck(symbolTable))
    val errors = (baseResult +: opResults.toSeq).collect { case Left(error) => error }
    
    if (errors.isEmpty) Right(()) else Left(errors.mkString("\n"))
  }
}

object BoolOperations {
  def createAnd(head: BoolExpression, tail: Seq[BoolExpression]): BoolExpression =
    if (tail.isEmpty) head
    else
      new BoolOperations(head, tail.map { expr => (BooleanOperator.And, expr) }: _*)

  def createOr(head: BoolExpression, tail: Seq[BoolExpression]): BoolExpression =
    if (tail.isEmpty) head
    else
      new BoolOperations(head, tail.map { expr => (BooleanOperator.Or, expr) }: _*)
}