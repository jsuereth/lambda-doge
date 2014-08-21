package doge.compiler

import java.io.File

import doge.compiler.ast.{LetExpr, DogeAst}
import doge.compiler.backend.GenerateClassFiles
import doge.compiler.types._

object Compiler {
  // Built in types
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


  val builtInTypes = TypeSystem.dumbEnvironment(Map(
    "Plus" -> TypeSystem.Function(TypeSystem.Integer, TypeSystem.Function(TypeSystem.Integer, TypeSystem.Integer)),
    "IS" -> idType,
    "PrintLn" -> printlnType
  ) ++
    // TODO - Some better semantic here.
    std.BuiltInType.all.typeTable)

  /** A very simple example of compiling DOGE script. */
  def compile(f: File, verbose: Boolean): File = {
    val s = scala.io.Source.fromFile(f)
    try {
      val input = s.getLines.mkString("\n")
      compile(input, f.getParentFile, rawName(f), verbose)
    } finally s.close()
  }

  def compile(input: String, classDirectory: File, name: String, verbose: Boolean): File = {
    def log(msg: String): Unit = if(verbose) System.err.println(msg)
    log(s"Compiling [$name]...")
    val parsed = parser.DogeParser.parseProgram(input)
    log(s"  -- Parsed --\n${parsed.mkString("\n")}")
    val typed = typeWithGlobalLet(parsed).collect({ case l: LetExprTyped => l })
    log(s"  -- Typed--\n${typed.mkString("\n")}")
    val clsFile = GenerateClassFiles.makeClassfile(typed, classDirectory, name)
    clsFile
  }

  private def typeWithGlobalLet(program: Seq[DogeAst]): Seq[TypedAst] = {
    val zero = (builtInTypes, Seq.empty[TypedAst])
    // This is a complete hack to share let environment since we INVERTED the let syntax for Doge.
    // Ideally we track this in some other manner.
    program.foldLeft(zero) { case ((env, typeResults), nextAst) =>
      val typedTree = Typer.typeTree(nextAst, env)
      val nextEnv = nextAst match {
        case LetExpr(id, _, _, _) =>
          env.withAdded(id -> typedTree.tpe)
        case _ => env
      }
      (nextEnv, typeResults :+ typedTree)
    }._2
  }


  def rawName(f: File): String = {
    val simple = f.getName
    simple.takeWhile(_ != '.')
  }

  def main(args: Array[String]): Unit = {
   /* def errorCarrot(pos: Position): String = {
      import pos._
      lineContents.take(column - 1).map { x => if (x == '\t') x else ' '} + "^"
    }*/
    val verbose = args.exists(_ == "-v")

    for(arg <- args.filterNot(_ == "-v")) {
      val f = new File(arg)
      try compile(f, verbose)
      catch {
        case ste: SyntaxTypeError  =>
          System.err.println(s"[ERROR] ${arg}@${ste.pos.line}:${ste.pos.column} - ${ste.msg}\n${ste.pos.longString}")
      }
    }
  }

}
