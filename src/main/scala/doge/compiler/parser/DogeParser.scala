package doge.compiler.parser


import doge.compiler.backend.GenerateClassFiles
import doge.compiler.types.{LetExprTyped, TypedAst, TypeSystem, Typer}

import scala.util.parsing.combinator.RegexParsers
import doge.compiler.ast._

object DogeParser extends RegexParsers {

  def parseProgram(input: String) = {
    parseAll(rep(expr), input) match {
      case Success(result,_) => result
      case Failure(msg, next) => sys.error(s"Failure: $msg, at ${next.pos.line}:${next.pos.column}")
      case Error(msg, next) => sys.error(s"Error: $msg, at ${next.pos.line}:${next.pos.column}")
    }
  }

  lazy val SO = literal("SO")
  lazy val WOW = literal("WOW")
  lazy val MANY = literal("MANY")
  lazy val VERY = literal("VERY")
  lazy val MUCH = literal("MUCH")
  lazy val SUCH = literal("SUCH")
  lazy val EXCL = literal("!")

  // TODO - define these somewhere else so they're not duplicated.
  def isSafeId(n: String) =
    !Set("WOW", "SO", "MANY", "VERY", "SUCH", "!", "MUCH").contains(n)

  lazy val idRaw: Parser[String] = "[^!\\W]+".r

  lazy val id: Parser[String] = idRaw ^? {
    case name if isSafeId(name) => name
  }

  lazy val idRef: Parser[IdReference] =
    positioned(id map { name => IdReference(name) })

  // TODO - Fail to parse invalid literals.
  lazy val intLiteral: Parser[IntLiteral] =
    positioned("\\d+".r map { value => IntLiteral(value.toInt) })

  lazy val argList: Parser[Seq[String]] =
    SO ~> rep(id)

  // TODO - Real type semantics...
  lazy val typeList: Parser[Seq[String]] =
    SUCH ~> rep(id)


  lazy val letExpr: Parser[LetExpr] =
    positioned(WOW ~ id ~ opt(typeList) ~ opt(argList) ~ apExpr ^^ {
       case ignore ~ id ~ types ~ args ~ result => LetExpr(id, types.getOrElse(Nil), args.getOrElse(Nil), result)
     })
  lazy val apExpr: Parser[ApExpr] =
    positioned((((MANY | VERY | MUCH) ~> idRef ) ~ rep(expr) <~ EXCL) ^^ {
      case id ~ args => ApExpr(id, args)
    })

  lazy val literal: Parser[Literal] = intLiteral

  lazy val expr: Parser[DogeAst] =
    (letExpr | apExpr | literal | idRef)

}
