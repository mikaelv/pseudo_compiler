package pseudoc.ast

case class StringConcat(values: Seq[Expression[_]]) extends Expression[String]

