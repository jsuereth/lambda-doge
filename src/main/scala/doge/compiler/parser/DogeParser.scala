package doge.compiler.parser


import doge.compiler.backend.GenerateClassFiles
import doge.compiler.types.{LetExprTyped, TypedAst, TypeSystem, Typer}

import scala.util.parsing.combinator.RegexParsers
import doge.compiler.ast._

object DogeParser extends RegexParsers {

  def parseModule(input: String, name: String): Module = {
    parseAll(rep(letExpr), input) match {
      case Success(result,_) => Module(name, result)
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

  // Helper to convert a parsed type into our AST types.
  def extractType(ast: ParseTypeAst): TypeSystem.Type = {
    var vars = Map.empty[String, TypeSystem.TypeVariable]
    def makeVar(name: String): TypeSystem.Type = {
      vars.getOrElse(name, {
        val result = TypeSystem.newVariable
        vars = vars + (name -> result)
        result
      })
    }
    def extractImpl(ast: ParseTypeAst): TypeSystem.Type =
      ast match {
        case TypeVar(name) => makeVar(name)
        case TypeCons(id, args) => TypeSystem.TypeConstructor(id, args.map(extractImpl))
      }
    extractImpl(ast)
  }

  lazy val typeParser =
     typeFull ^^ extractType
  lazy val typeFull = (typeFunction | typeRaw)
  // Type parsing. TODO - figuring out functions will be tough..
  lazy val typeFunction: Parser[ParseTypeAst] =
    (typeRaw <~ ("=>" | TypeSystem.FUNCTION_TCONS_NAME)) ~ typeFull ^^ {
      case arg ~ result => TypeCons(TypeSystem.FUNCTION_TCONS_NAME, Seq(arg, result))
    }
  lazy val typeRaw: Parser[ParseTypeAst] =
    (grouped | typeVar | typeConstructor)

  lazy val typeConstructor: Parser[TypeCons] =
     typeId ~ opt("[" ~> typeRaw.* <~ "]") ^^ {
       case id ~ args => TypeCons(id, args.getOrElse(Nil))
     }
  lazy val grouped: Parser[ParseTypeAst] = "(" ~> typeFull <~ ")"
  lazy val typeVar: Parser[TypeVar] = id ^? {
    // TODO - handle locales correctly
    case name if name.forall(_.isLower) => TypeVar(name)
  }
  lazy val typeId: Parser[String] = id ^? {
    // TODO - handle locales correctly
    case name if !name.forall(_.isLower) => name
  }



  lazy val idRef: Parser[IdReference] =
    positioned(id map { name => IdReference(name) })

  // TODO - Fail to parse invalid literals.
  lazy val intLiteral: Parser[IntLiteral] =
    positioned("\\d+".r map { value => IntLiteral(value.toInt) })

  lazy val boolLiteral: Parser[BoolLiteral] =  {
    val t = literal("true") ^^^ BoolLiteral(true)
    val f = literal("false") ^^^ BoolLiteral(false)
    positioned(t | f)
  }

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

  lazy val literal: Parser[Literal] = intLiteral | boolLiteral

  lazy val expr: Parser[DogeAst] =
    (letExpr | apExpr | literal | idRef)

}
