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
case class IdReference(name: String) extends DogeAst {
  override def toString = s"<$name>"
}

object RawLambda {

  abstract class SyntaxNode
  case class Lambda(v: String, body: SyntaxNode) extends SyntaxNode
  case class Ident(name: String) extends SyntaxNode
  case class Apply(fn: SyntaxNode, arg: SyntaxNode) extends SyntaxNode
  case class Let(v: String, defn: SyntaxNode, body: SyntaxNode) extends SyntaxNode
  case class Letrec(v: String, defn: SyntaxNode, body: SyntaxNode) extends SyntaxNode

  object SyntaxNode {
    def string(ast: SyntaxNode): String = {
      if (ast.isInstanceOf[Ident])
        nakedString(ast)
      else
        "(" + nakedString(ast) + ")"
    }

    def nakedString(ast: SyntaxNode) = ast match {
      case i: Ident => i.name
      case l: Lambda => "fn " + l.v + " ⇒ " + string(l.body)
      case f: Apply => string(f.fn) + " " + string(f.arg)
      case l: Let => "let " + l.v + " = " + string(l.defn) + " in " + string(l.body)
      case l: Letrec => "letrec " + l.v + " = " + string(l.defn) + " in " + string(l.body)
    }
  }

}
