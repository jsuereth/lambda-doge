package doge.compiler.ast


import scala.util.parsing.input.Positional


// Bare minimum
sealed abstract class DogeAst extends Positional

case class LetExpr(name: String, types: Seq[String], argNames: Seq[String], definition: DogeAst) extends DogeAst {
  private def argList =
    if(!types.isEmpty) for((name, tpe) <- argNames zip types) yield s"$name: $tpe"
    else argNames map (n => s"$n: ???")
  override def toString = s"let $name(${argList.mkString(", ")}) = $definition"
}
case class ApExpr(name: IdReference, args: Seq[DogeAst]) extends DogeAst {
  override def toString =
    if(args.isEmpty) name.toString
    else s"($name ${args.mkString(" ")})"
}
sealed abstract class Literal extends DogeAst
case class IntLiteral(value: Int) extends Literal {
  override def toString = value.toString
}
case class BoolLiteral(value: Boolean) extends Literal {
  override def toString = value.toString
}
case class IdReference(name: String) extends DogeAst {
  override def toString = s"<$name>"
}

case class Module(name: String, definitions: Seq[LetExpr]) extends DogeAst {
  override def toString = s"module $name {${definitions.mkString("\n  ","\n  ", "\n")}}"
}
