package doge.compiler.types

import doge.compiler.ast._
import TypeSystem.Type
import scala.util.parsing.input.{NoPosition, Position}

/** Reimplementation of the AST, but with types. */
sealed trait TypedAst {
  def tpe: Type
  def pos: Position
}
case class LetExprTyped(name: String, argNames: Seq[String], definition: TypedAst, tpe: Type, pos: Position = NoPosition) extends TypedAst {
  def returnType: Type = definition.tpe

  def argTypes: Seq[Type] = {
    def argTypeImpl(args: List[Type], tpe: Type, remaining: Int): Seq[Type] =
      if(remaining == 0) args.reverse
      else tpe match {
        case TypeSystem.Function(arg, to) => argTypeImpl(arg :: args, to, remaining-1)
        case _ => throw new SyntaxTypeError(pos, s"Let expression type error, not enough arguments.  Expected ${argNames.size}, found type: $tpe")
      }
    argTypeImpl(Nil, tpe, argNames.size)
  }
  private def argList = argNames map (n => s"$n: ???")
  override def toString =
    s"""|let $name(${argNames.mkString(", ")}) :: $tpe
        |    $name(${argNames.mkString(", ")})  =
        |      $definition""".stripMargin
}
case class ApExprTyped(name: IdReferenceTyped, args: Seq[TypedAst], tpe: Type, pos: Position = NoPosition) extends TypedAst {
  override def toString =
    if(args.isEmpty) s"$name"
    else s"$name(${args.mkString(", ")})[$tpe]"
}
sealed abstract class LiteralTyped extends TypedAst
case class IntLiteralTyped(value: Int, pos: Position = NoPosition) extends LiteralTyped {
  override def toString = s"$value[int]"
  override val tpe = TypeSystem.Integer
}
case class BoolLiteralTyped(value: Boolean, pos: Position = NoPosition) extends LiteralTyped {
  override def toString = s"$value[bool]"
  override def tpe = TypeSystem.Bool
}

case class IdReferenceTyped(name: String, env: TypeEnvironmentInfo, pos: Position = NoPosition) extends TypedAst {
  override def tpe = env.tpe
  override def toString = s"<${env.location}>$name[$tpe]</${env.location}>"
}

case class ModuleTyped(name: String, definitions: Seq[LetExprTyped]) extends TypedAst {
  override val tpe = TypeSystem.Unit
  override val pos = NoPosition
  override def toString = s"module $name {${definitions.mkString("\n  ","\n\n  ", "\n")}}"
}

