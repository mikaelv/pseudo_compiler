package pseudoc.ast




sealed trait IntExpression


case class IntRef(varName: String) extends IntExpression

case class IntLiteral(value: Int) extends IntExpression


// TODO use IntExpression, StringExpression: better pattern matching

enum MultDivOperator:
  case Mult, Div

case class IntMultDiv(base: IntExpression, ops: Seq[(MultDivOperator, IntExpression)]) extends IntExpression

object IntMultDiv {
  def create(head: IntExpression, tail: Seq[(String, IntExpression)]): IntMultDiv =
    new IntMultDiv(head, tail.map {
      case ("*", expr) => (MultDivOperator.Mult, expr)
      case ("/", expr) => (MultDivOperator.Div, expr)
      case (op, expr) => throw new RuntimeException("Invalid operator: " + op)
    })
}


enum AddSubOperator:
  case Add, Sub

case class IntAddSub(base: IntMultDiv, ops: Seq[(AddSubOperator, IntMultDiv)]) extends IntExpression

object IntAddSub {
  def create(head: IntMultDiv, tail: Seq[(String, IntMultDiv)]): IntAddSub =
    new IntAddSub(head, tail.map {
      case ("+", expr) => (AddSubOperator.Add, expr)
      case ("-", expr) => (AddSubOperator.Sub, expr)
      case (op, expr) => throw new RuntimeException("Invalid operator: " + op)
    })
}

