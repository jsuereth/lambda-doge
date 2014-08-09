package doge.compiler.parser


import doge.compiler.backend.GenerateClassFiles
import doge.compiler.types.{LetExprTyped, TypedAst, TypeSystem, Typer}

import scala.util.parsing.combinator.RegexParsers
import doge.compiler.ast._

object DogeParser extends RegexParsers {

  def main(args: Array[String]): Unit = {

    val testWow = """|WOW
                    |Big
                    |SO numbers
                    |MUCH Plus numbers 1 !""".stripMargin
    val testProgram = """|WOW
                        |Big
                        |SUCH Int
                        |SO numbers
                        |MUCH Plus numbers 1!
                        |
                        |WOW
                        |Doge
                        |MUCH IS 5!
                        |
                        |WOW
                        |Katz
                        |VERY Big Doge!
                        |
                        |WOW
                        |Rain
                        |VERY Katz!
                        |
                        |WOW
                        |main
                        |MUCH PrintLn
                        |VERY Big Doge!
                        |MUCH Rain!
                        |VERY Katz!!""".stripMargin
    //test(rep(expr), testProgram)(_.mkString("\n"))
    val idType = {
      val var1 = TypeSystem.newVariable
      TypeSystem.Function(var1, var1)
    }
    // A hack just to make samples compile.
    // Ideally we expand the type system with a notion of "varargs" or some such.
    val printlnType = {
      TypeSystem.FunctionN(
        TypeSystem.Unit,
        TypeSystem.newVariable,
        TypeSystem.newVariable,
        TypeSystem.newVariable
      )
    }
    val cheaterEnv = TypeSystem.dumbEnvironment(Map(
      "Plus" -> TypeSystem.Function(TypeSystem.Integer, TypeSystem.Function(TypeSystem.Integer, TypeSystem.Integer)),
      "IS" -> idType,
      "PrintLn" -> printlnType
    ))
    test(rep(expr), testProgram) { program =>
      println(program.mkString("\n"))
      println(" -- TYPED --")
      val zero = (cheaterEnv, Seq.empty[TypedAst])
      // This is a complete hack to share let environment since we INVERTED the let syntax for Doge
      val typeTrees = program.foldLeft(zero) { case ((env, typeResults), nextAst) =>
        val typedTree = Typer.typeTree(nextAst, env)
        val nextEnv = nextAst match {
          case LetExpr(id, _, _, _) =>
            env.withAdded(id -> typedTree.tpe)
          case _ => env
        }
        (nextEnv, typeResults :+ typedTree)
      }._2
      println(typeTrees.mkString("\n"))
      //println(program.zip(types).map { case (ast, t) => s"$ast :: $t"}.mkString("\n"))
      println("Types are sound!")
      println(" -- EVALUATED -- ")
      doge.compiler.interpreter.Interpreter.interpret(program)
      println(" -- Compiled -- ")
      GenerateClassFiles.makeClassfile(
        typeTrees.collect({ case l: LetExprTyped => l }),
        new java.io.File("."),
        "test"
      )
      "Program complete!"
    }
  }

  def test[T](p: Parser[T], expr: String)(show: T => String = (x: T) => x.toString): Unit = {
    println("--- TEST ---")
    println(" -- INPUT --")
    println(expr)
    println(" -- PARSED --")
    parseAll(p, expr) match {
      case Success(result,_) => println(show(result))
      case Failure(msg, next) => println(s"Failure: $msg, at ${next.pos.line}:${next.pos.column}")
      case Error(msg, next) => println(s"Error: $msg, at ${next.pos.line}:${next.pos.column}")
    }
    println(" -- END --")
  }

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

  // SO <id>*
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
