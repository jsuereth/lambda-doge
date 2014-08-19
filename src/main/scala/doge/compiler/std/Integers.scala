package doge.compiler
package std

import doge.compiler.backend.MethodWriterState
import doge.compiler.std
import doge.compiler.types.TypeSystem.Type
import doge.compiler.types.{ApExprTyped, TypeSystem, TypedAst}
import org.objectweb.asm.signature.SignatureVisitor
import org.objectweb.asm.Opcodes._

import scalaz._

/** Built in integer operations.  Most integer operations are directly in bytecode. */
object Integers extends BuiltInType {
  def name: String = "Integer"

  val PLUS = "Plus"
  val MINUS = "Minus"
  val MUL = "Multiply"
  val DIV = "Divide"

  def typeTable: Map[String, Type] =
    Map(
      PLUS -> TypeSystem.Function(TypeSystem.Integer, TypeSystem.Function(TypeSystem.Integer, TypeSystem.Integer)),
      MINUS -> TypeSystem.Function(TypeSystem.Integer, TypeSystem.Function(TypeSystem.Integer, TypeSystem.Integer)),
      MUL -> TypeSystem.Function(TypeSystem.Integer, TypeSystem.Function(TypeSystem.Integer, TypeSystem.Integer)),
      DIV -> TypeSystem.Function(TypeSystem.Integer, TypeSystem.Function(TypeSystem.Integer, TypeSystem.Integer))
    )

  def backend: PartialFunction[TypedAst, State[MethodWriterState, Unit]] = {
    case ApExprTyped(id, Seq(left, right), _) if id.name == "Plus" => plus(left, right)
    case ApExprTyped(id, Seq(left, right), _) if id.name == "Minus" => minus(left, right)
    case ApExprTyped(id, Seq(left, right), _) if id.name == "Multiply" => mult(left, right)
    case ApExprTyped(id, Seq(left, right), _) if id.name == "Divide" => divide(left, right)
  }

  def visitSignatureInternal: PartialFunction[(SignatureVisitor, Type), Unit] = {
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