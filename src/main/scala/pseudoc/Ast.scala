package pseudoc

sealed trait Ast {


}

object Ast {
  case class Algorithm(name: String) extends Ast
  case class Variables(vars: Seq[VariableDecl])
  case class VariableDecl(name: String, tpe: String)

}

