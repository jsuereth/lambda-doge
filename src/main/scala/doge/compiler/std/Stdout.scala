package doge.compiler.std


import doge.compiler.backend.{MethodWriter, MethodWriterState}
import doge.compiler.std
import doge.compiler.symbols.{BuiltInSymbolTable, SymbolTable}
import doge.compiler.types.TypeSystem.{TypeConstructor, Type, newVariable, FunctionN, Function}
import doge.compiler.types._
import org.objectweb.asm.Label
import org.objectweb.asm.signature.SignatureVisitor
import org.objectweb.asm.Opcodes._

import scalaz._


object StdOut extends BuiltInType {

  def name: String = "Stdout"

  val Print = "Print"
  val PrintLn = "PrintLn"

  val printType = {
    val arg = newVariable
    FunctionN(
      TypeSystem.Unit, 
      arg
    )
  }
  val printLnType = {
    val arg = newVariable
    FunctionN(
      TypeSystem.Unit, 
      arg
    ) 
  }

  override val symbolTable: SymbolTable =
     new BuiltInSymbolTable(Seq(
       BuiltInSymbolTable.Function(Print, printType),
       BuiltInSymbolTable.Function(PrintLn, printLnType)))

  override val backend: PartialFunction[TypedAst, State[MethodWriterState, Unit]] = {
    case ApExprTyped(i, Seq(arg), tpe, pos) if i.name == Print => 
       printImpl(arg)
       case ApExprTyped(i, Seq(arg), tpe, pos) if i.name == PrintLn => 
       printLnImpl(arg)
  }


  import MethodWriter._
  def stdout = getStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
  def getRuntimeType(tpe: Type): Type =
    tpe match {
      case TypeSystem.Integer | TypeSystem.Bool => tpe
      case _ => TypeSystem.Simple("java.lang.Object")
    }
  private def printImpl(arg: TypedAst): State[MethodWriterState, Unit] = {
    for {
      out <- stdout
      _ <- placeOnStack(arg)
      _ <- invokeVirtual("java/io/PrintStream", "print", 1,
        TypeSystem.Function(getRuntimeType(arg.tpe), TypeSystem.Unit), false)
    } yield ()
  }
  private def printLnImpl(arg: TypedAst): State[MethodWriterState, Unit] = {
    for {
      out <- stdout
      _ <- placeOnStack(arg)
      _ <- invokeVirtual("java/io/PrintStream", "println", 1,
        TypeSystem.Function(getRuntimeType(arg.tpe), TypeSystem.Unit), false)
    } yield ()
  }  
}
