package doge.compiler
package std

import doge.compiler.backend.MethodWriterState
import doge.compiler.std
import doge.compiler.symbols.{BuiltInSymbolTable, SymbolTable}
import doge.compiler.types.TypeSystem.Type
import doge.compiler.types._
import org.objectweb.asm.signature.SignatureVisitor
import org.objectweb.asm.Opcodes._

import scalaz._

/** Built in integer operations.  Most integer operations are directly in bytecode. */
object Integers extends BuiltInType {
  def name: String = "Integer"

  val BinaryIntOp = TypeSystem.Function(TypeSystem.Integer, TypeSystem.Function(TypeSystem.Integer, TypeSystem.Integer))
  val PLUS = "Plus"
  val PlusSym = BuiltInSymbolTable.Function(PLUS, BinaryIntOp)
  val MINUS = "Minus"
  val MUL = "Multiply"
  val DIV = "Divide"

  override val symbolTable: SymbolTable =
    new BuiltInSymbolTable(Seq(
      PlusSym,
      BuiltInSymbolTable.Function(MINUS, BinaryIntOp),
      BuiltInSymbolTable.Function(MUL, BinaryIntOp),
      BuiltInSymbolTable.Function(DIV, BinaryIntOp)))

  override val backend: PartialFunction[TypedAst, State[MethodWriterState, Unit]] = {
    case ApExprTyped(id, Seq(left, right), _, _) if id.name == "Plus" => plus(left, right)
    case ApExprTyped(id, Seq(left, right), _, _) if id.name == "Minus" => minus(left, right)
    case ApExprTyped(id, Seq(left, right), _, _) if id.name == "Multiply" => mult(left, right)
    case ApExprTyped(id, Seq(left, right), _, _) if id.name == "Divide" => divide(left, right)
  }

  override val visitSignatureInternal: PartialFunction[(SignatureVisitor, Type), Unit] = {
    case (sv, TypeSystem.Integer) => sv.visitBaseType('I')
  }


  import doge.compiler.backend.MethodWriter._
  def plus(left: TypedAst, right: TypedAst): State[MethodWriterState, Unit] =
    intInsn(IADD, left, right)

  def minus(left: TypedAst, right: TypedAst): State[MethodWriterState, Unit] =
    intInsn(ISUB, left, right)

  def mult(left: TypedAst, right: TypedAst): State[MethodWriterState, Unit] =
    intInsn(IMUL, left, right)


  def divide(left: TypedAst, right: TypedAst): State[MethodWriterState, Unit] =
    intInsn(IDIV, left, right)

  private def intInsn(code: Int, left: TypedAst, right: TypedAst): State[MethodWriterState, Unit] =
    for {
      _ <- placeOnStack(left)
      _ <- placeOnStack(right)
      _ <- rawInsn(_.visitInsn(code))
    } yield ()

}