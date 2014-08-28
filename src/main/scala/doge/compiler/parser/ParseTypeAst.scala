package doge.compiler.parser


trait ParseTypeAst
case class TypeVar(n: String) extends ParseTypeAst
case class TypeCons(n: String, args: Seq[ParseTypeAst]) extends ParseTypeAst