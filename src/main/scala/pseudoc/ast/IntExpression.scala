package pseudoc.ast

import pseudoc.SymbolTable

sealed trait IntExpression extends TypedExpression[Int] {
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
  ): IntMultDiv =
    // TODO if tail.isEmpty head
    new IntMultDiv(
      head,
      tail.map {
        case ("*", expr) => (MultDivOperator.Mult, expr)
        case ("/", expr) => (MultDivOperator.Div, expr)
        case (op, expr) => throw new RuntimeException("Invalid operator: " + op)
      }
    )
}

enum AddSubOperator:
  case Add, Sub

case class IntAddSub(base: IntMultDiv, ops: Seq[(AddSubOperator, IntMultDiv)])
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
  def create(head: IntMultDiv, tail: Seq[(String, IntMultDiv)]): IntAddSub =
    new IntAddSub(
      head,
      tail.map {
        case ("+", expr) => (AddSubOperator.Add, expr)
        case ("-", expr) => (AddSubOperator.Sub, expr)
        case (op, expr) => throw new RuntimeException("Invalid operator: " + op)
      }
    )
}