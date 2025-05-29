package pseudoc.ast



trait Expression[A]

case class StringLiteral(value: String) extends Expression[String]

case class StringRef(varName: String) extends Expression[String]

case class IntRef(varName: String) extends Expression[Int]

case class IntLiteral(value: Int) extends Expression[Int]


// TODO use IntExpression, StringExpression: better pattern matching

enum MultDivOperator:
  case Mult, Div

case class IntMultDiv(base: Expression[Int], ops: Seq[(MultDivOperator, Expression[Int])]) extends Expression[Int]

object IntMultDiv {
  def create(head: Expression[Int], tail: Seq[(String, Expression[Int])]): IntMultDiv =
    new IntMultDiv(head, tail.map {
      case ("*", expr) => (MultDivOperator.Mult, expr)
      case ("/", expr) => (MultDivOperator.Div, expr)
      case (op, expr) => throw new RuntimeException("Invalid operator: " + op)
    })
}


enum AddSubOperator:
  case Add, Sub

case class IntAddSub(base: IntMultDiv, ops: Seq[(AddSubOperator, IntMultDiv)]) extends Expression[Int]

object IntAddSub {
  def create(head: IntMultDiv, tail: Seq[(String, IntMultDiv)]): IntAddSub =
    new IntAddSub(head, tail.map {
      case ("+", expr) => (AddSubOperator.Add, expr)
      case ("-", expr) => (AddSubOperator.Sub, expr)
      case (op, expr) => throw new RuntimeException("Invalid operator: " + op)
    })
}

