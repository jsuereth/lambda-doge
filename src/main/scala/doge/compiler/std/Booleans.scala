package doge.compiler.std


import doge.compiler.backend.{MethodWriter, MethodWriterState}
import doge.compiler.std
import doge.compiler.types.TypeSystem.Type
import doge.compiler.types._
import org.objectweb.asm.Label
import org.objectweb.asm.signature.SignatureVisitor
import org.objectweb.asm.Opcodes._

import scalaz._


object Booleans extends BuiltInType {

  def name: String = "Boolean"

  val IF = "ifs"


  val ifType = {
    val result = TypeSystem.newVariable
    TypeSystem.FunctionN(
      result,
      TypeSystem.Bool,
      result,
      result
    )
  }

  override val typeTable: Seq[TypeEnvironmentInfo] =
    Seq(
      TypeEnvironmentInfo(IF, BuiltIn, ifType)
    )


  override val backend: PartialFunction[TypedAst, State[MethodWriterState, Unit]] = {
    case ApExprTyped(i, Seq(check, left, right), tpe, pos) if i.name == IF => ifs(check, left, right, tpe)
  }


  override val visitSignatureInternal: PartialFunction[(SignatureVisitor, Type), Unit] = {
    case (sv, TypeSystem.Bool) => sv.visitBaseType('Z')
  }




  /** Encoding on the JVM of "ifs" method, a.k.a. IF statement. */
  def ifs(check: TypedAst, left: TypedAst, right: TypedAst, resultType: Type): State[MethodWriterState, Unit] = {
    import MethodWriter._
    val fLabel = new Label()
    val fEndLabel = new Label()
    for {
      _ <- placeOnStack(check)
      _ <- jumpIfEq(fLabel)
      _ <- placeOnStack(left)
      _ <- goto(fEndLabel)
      _ <- writeLabel(fLabel)
      _ <- placeOnStack(right)
      _ <- writeLabel(fEndLabel)
    } yield ()
  }
}
