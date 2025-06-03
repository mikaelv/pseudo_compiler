package pseudoc.ast

import pseudoc.SymbolTable

// Yes I know this file is huge. If I split it, no sealed traits ! :-(

/**
 * Base trait for all expressions with static type checking
 */
sealed trait Expression {
  /**
   * Check if this expression is type-correct
   * @return Either an error message or Unit if type checking passes
   */
  def typeCheck(symbolTable: SymbolTable): Either[String, Unit]
}

/**
 * Trait for expressions with a known return type
 */
sealed trait TypedExpression[T] extends Expression {
  /**
   * Get the class object for the expression type
   */
  def expressionType: Class[T]
}

sealed trait VariableRef[T] extends TypedExpression[T]


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
    symbolTable.checkBoolVariable(varName)
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



sealed trait IntExpression extends VariableRef[Int] {
  override def expressionType: Class[Int] = classOf[Int]
}

case class IntRef(varName: String) extends IntExpression {
  override def typeCheck(symbolTable: SymbolTable): Either[String, Unit] = {
    symbolTable.checkIntVariable(varName)
  }
}

case class IntLiteral(value: Int) extends IntExpression {
  // Int literals are always type-correct
  override def typeCheck(symbolTable: SymbolTable): Either[String, Unit] = Right(())
}

enum MultDivOperator:
  case Mult, Div

case class IntMultDiv(
                       base: IntExpression,
                       ops: Seq[(MultDivOperator, IntExpression)]
                     ) extends IntExpression {
  override def typeCheck(symbolTable: SymbolTable): Either[String, Unit] = {
    // Check base expression
    val baseResult = base.typeCheck(symbolTable)

    // Check all operands
    val opResults = ops.map(_._2.typeCheck(symbolTable))
    val errors = (baseResult +: opResults).collect { case Left(error) => error }

    if (errors.isEmpty) Right(()) else Left(errors.mkString("\n"))
  }
}

object IntMultDiv {
  def create(
              head: IntExpression,
              tail: Seq[(String, IntExpression)]
            ): IntExpression =
    if (tail.isEmpty)
      head
    else
      new IntMultDiv(
        head,
        tail.map {
          case ("*", expr) => (MultDivOperator.Mult, expr)
          case ("/", expr) => (MultDivOperator.Div, expr)
          case (op, expr)  => throw new RuntimeException("Invalid operator: " + op)
        }
      )
}

enum AddSubOperator:
  case Add, Sub

case class IntAddSub(base: IntExpression, ops: Seq[(AddSubOperator, IntExpression)])
  extends IntExpression {
  override def typeCheck(symbolTable: SymbolTable): Either[String, Unit] = {
    // Check base expression
    val baseResult = base.typeCheck(symbolTable)

    // Check all operands
    val opResults = ops.map(_._2.typeCheck(symbolTable))
    val errors = (baseResult +: opResults).collect { case Left(error) => error }

    if (errors.isEmpty) Right(()) else Left(errors.mkString("\n"))
  }
}

object IntAddSub {
  def create(head: IntExpression, tail: Seq[(String, IntExpression)]): IntExpression =
    if (tail.isEmpty)
      head
    else
      new IntAddSub(
        head,
        tail.map {
          case ("+", expr) => (AddSubOperator.Add, expr)
          case ("-", expr) => (AddSubOperator.Sub, expr)
          case (op, expr)  => throw new RuntimeException("Invalid operator: " + op)
        }
      )
}

case class ArrayAccess(arrayExpr: ArrayExpression, indexExpr: IntExpression) extends IntExpression {
  override def typeCheck(symbolTable: SymbolTable): Either[String, Unit] = {
    val arrayResult = arrayExpr.typeCheck(symbolTable)
    val indexResult = indexExpr.typeCheck(symbolTable)
    val errors = Seq(arrayResult, indexResult).collect { case Left(error) => error }
    if (errors.isEmpty) Right(()) else Left(errors.mkString("\n"))
  }
}



sealed trait StringExpression extends TypedExpression[String] {
  override def expressionType: Class[String] = classOf[String]
}

case class StringConcat(values: Seq[StringExpression]) extends StringExpression {
  override def typeCheck(symbolTable: SymbolTable): Either[String, Unit] = {
    // Check each component of the concatenation
    val results = values.map(_.typeCheck(symbolTable))
    val errors = results.collect { case Left(error) => error }

    if (errors.isEmpty) Right(()) else Left(errors.mkString("\n"))
  }
}

object StringConcat {
  def apply(values: Seq[StringExpression]): StringExpression = {
    require(values.nonEmpty)
    if (values.length == 1)
      values.head
    else
      new StringConcat(values)
  }
}

case class StringLiteral(value: String) extends StringExpression {
  // String literals are always type-correct
  override def typeCheck(symbolTable: SymbolTable): Either[String, Unit] = Right(())
}

case class StringRef(varName: String) extends StringExpression {
  override def typeCheck(symbolTable: SymbolTable): Either[String, Unit] = {
    // Strict type checking - only allow reference to string variables
    symbolTable.checkStringVariable(varName)
  }
}


sealed trait ArrayExpression extends TypedExpression[Array[Int]] {
  override def expressionType: Class[Array[Int]] = classOf[Array[Int]]
}

case class ArrayRef(varName: String) extends ArrayExpression {
  override def typeCheck(symbolTable: SymbolTable): Either[String, Unit] = {
    symbolTable.checkArrayIntVariable(varName)
  }
}

case class ArrayLiteral(values: Seq[IntExpression]) extends ArrayExpression {
  override def typeCheck(symbolTable: SymbolTable): Either[String, Unit] = {
    val errors = values.map(_.typeCheck(symbolTable)).collect { case Left(error) => error }
    if (errors.isEmpty) Right(()) else Left(errors.mkString("\n"))
  }
}

