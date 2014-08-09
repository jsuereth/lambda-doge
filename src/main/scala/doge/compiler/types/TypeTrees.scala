package doge.compiler.types

import doge.compiler.ast._
import TypeSystem.Type

/** Reimplementation of the AST, but with types. */
sealed trait TypedAst {
  def tpe: Type
}
case class LetExprTyped(name: String, argNames: Seq[String], definition: TypedAst, tpe: Type) extends TypedAst {
  private def argList = argNames map (n => s"$n: ???")
  override def toString =
    s"""|let $name(${argNames.mkString(", ")}) :: $tpe
        |    $name(${argNames.mkString(", ")})  =
        |      $definition""".stripMargin
}
case class ApExprTyped(name: IdReferenceTyped, args: Seq[TypedAst], tpe: Type) extends TypedAst {
  override def toString =
    if(args.isEmpty) s"$name"
    else s"$name(${args.mkString(", ")})[$tpe]"
}
sealed abstract class LiteralTyped extends TypedAst
case class IntLiteralTyped(value: Int) extends LiteralTyped {
  override def toString = s"$value[int]"
  override val tpe = TypeSystem.Integer
}
case class IdReferenceTyped(name: String, tpe: Type) extends TypedAst {
  override def toString = s"$name[$tpe]"
}
