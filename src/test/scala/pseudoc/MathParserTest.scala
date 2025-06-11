package pseudoc

import org.scalatest.funsuite.AnyFunSuiteLike
import fastparse._
import MathParser._

class MathParserTest extends AnyFunSuiteLike:
  test("expressions") :
    val Parsed.Success(2, _) = parse("1+1", expr(_))
    val Parsed.Success(15, _) = parse("(1+1*2)+3*4", expr(_))
    val Parsed.Success(21, _) = parse("((1+1*2)+(3*4*5))/3", expr(_))
    val Parsed.Failure(expected, failIndex, extra) = parse("1+1*", expr(_))
    val longAggMsg = extra.trace().longAggregateMsg
    assert(
      failIndex == 4,
      longAggMsg ==
        """Expected expr:1:1 / addSub:1:1 / divMul:1:3 / factor:1:5 / (number | parens):1:5, found """""
    )


  def check(str: String, num: Int)=
    val Parsed.Success(value, _) = parse(str, expr(_))
    assert(value == num)

  test("expressions with whitespaces") :
     check("1+1", 2)
     check("1+   1*   2", 3)
     check("(1+   1  *  2)+(   3*4*5)", 63)
     check("15/3", 5)
     check("63  /3", 21)
     check("(1+    1*2)+(3      *4*5)/20", 6)
     check("((1+      1*2)+(3*4*5))/3", 21)

