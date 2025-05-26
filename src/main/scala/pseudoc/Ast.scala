package pseudoc

sealed trait Ast {


}


object Ast {
  sealed trait Instruction

  case class Algorithm(name: String) extends Ast
  case class Variables(vars: Seq[VariableDecl])
  case class VariableDecl(name: String, tpe: String)
  case class ForLoop(variable: String, start: Int, end: Int, instrs: Seq[Instruction]) extends Instruction

}

