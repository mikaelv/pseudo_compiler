package pseudoc

object MathParser {

  import fastparse._, JavaWhitespace._

  def number[$: P]: P[Int] = P(CharIn("0-9").rep(1).!.map(_.toInt))

  def parens[$: P]: P[Int] = P("(" ~/ addSub ~ ")")

  def factor[$: P]: P[Int] = P(number | parens)

  def divMul[$: P]: P[Int] = P(factor ~ (CharIn("*/").! ~/ factor).rep).map(eval)

  def addSub[$: P]: P[Int] = P(divMul ~ (CharIn("+\\-").! ~/ divMul).rep).map(eval)

  def expr[$: P]: P[Int] = P(addSub ~ End)

  def eval(tree: (Int, Seq[(String, Int)])) = {
    val (base, ops) = tree
    ops.foldLeft(base) { case (left, (op, right)) => op match {
      case "+" => left + right
      case "-" => left - right
      case "*" => left * right
      case "/" => left / right
    }
    }
  }
}
