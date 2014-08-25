package doge.compiler

import java.io.File

import doge.compiler.ast.{LetExpr, DogeAst}
import doge.compiler.backend.GenerateClassFiles
import doge.compiler.closures.ClosureLift
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


  import types._
  val builtInTypes = TypeEnv.dumbEnvironment(
    Seq(TypeEnvironmentInfo("IS", BuiltIn, idType), TypeEnvironmentInfo("PrintLn", BuiltIn, printlnType)) ++
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
    val parsed = parser.DogeParser.parseModule(input, name)
    log(s"  -- Parsed --\n${parsed}")
    val typed = Typer.typeFull(parsed, builtInTypes)
    log(s"  -- Typed--\n${typed}")
    val closured = ClosureLift.liftClosures(typed)
    log(s"  -- Closure-Lifted --\n${closured}")
    val clsFile = GenerateClassFiles.makeClassfile(typed, classDirectory)
    clsFile
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
