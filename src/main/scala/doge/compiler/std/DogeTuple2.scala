package doge.compiler
package std


import org.objectweb.asm.Opcodes._
import doge.compiler.types._
import org.objectweb.asm.signature.SignatureVisitor
import types.TypeSystem._
import backend._
import scalaz._
import Scalaz._



/** Built in Tuple2 types/namespace. */
object DogeTuple2 extends BuiltInType {

  // Actual names in the language
  val FIRST = "fst"
  val SECOND = "snd"
  val CONSTRUCTOR = "tuple2"
  // The name of the type.
  val name = "Tuple2"

  // Typing table, for running typer.
  override val typeTable = Seq[TypeEnvironmentInfo](
    TypeEnvironmentInfo(FIRST, BuiltIn, {
      val a = newVariable
      val b = newVariable
      Function(TypeConstructor(name, Seq(a, b)), a)
    }),
    TypeEnvironmentInfo(SECOND, BuiltIn, {
      val a = newVariable
      val b = newVariable
      Function(TypeConstructor(name, Seq(a, b)), b)
    }),
    TypeEnvironmentInfo(CONSTRUCTOR, BuiltIn, {
      val a = newVariable
      val b = newVariable
      FunctionN(TypeConstructor(name, Seq(a, b)), a, b)
    })
  )

  override val visitSignatureInternal: PartialFunction[(SignatureVisitor, Type), Unit] = {
    case (sv, TypeConstructor(`name`, Seq(arg1, arg2))) =>
        sv.visitArrayType().visitClassType("java/lang/Object;")
  }

  // Actual implementation of the methods exposed.
  override val backend: PartialFunction[TypedAst, State[MethodWriterState, Unit]] = {
    case ApExprTyped(IdReferenceTyped(CONSTRUCTOR, _, _), Seq(left, right), tpe, _) => constructorImpl(left, right)
    case ApExprTyped(IdReferenceTyped(FIRST, _, _), Seq(tuple), tpe, _) => fstMethodImpl(tuple)
    case ApExprTyped(IdReferenceTyped(SECOND, _, _), Seq(tuple), tpe, _) => fstMethodImpl(tuple)
  }

  private def fstMethodImpl(tuple: TypedAst): State[MethodWriterState, Unit] = {
    import MethodWriter._
    for {
      _ <- placeOnStack(tuple)
      _ <- rawInsn(_.visitLdcInsn(0))
      _ <- rawInsn(_.visitInsn(AALOAD))
      _ <- unbox(fstTupleType(tuple.tpe))
    } yield ()
  }


  private def sndMethodImpl(tuple: TypedAst): State[MethodWriterState, Unit] = {
    import MethodWriter._
    for {
      _ <- placeOnStack(tuple)
      _ <- rawInsn(_.visitLdcInsn(1))
      _ <- rawInsn(_.visitInsn(AALOAD))
      _ <- unbox(sndTupleType(tuple.tpe))
    } yield ()
  }

  private def fstTupleType(tpe: Type): Type =
    tpe match {
      case TypeConstructor(name, Seq(left, right)) => left
      case _ => sys.error(s"$tpe is not a $name!")
    }
  private def sndTupleType(tpe: Type): Type =
    tpe match {
      case TypeConstructor(name, Seq(left, right)) => right
      case _ => sys.error(s"$tpe is not a $name!")
    }

  private def constructorImpl(left: TypedAst, right: TypedAst): State[MethodWriterState, Unit] = {
    import MethodWriter._
    for {
      _ <- rawInsn(_.visitLdcInsn(2))
      _ <- rawInsn(_.visitTypeInsn(ANEWARRAY, "java/lang/Object"))
      _ <- dupe
      _ <- dupe
      _ <- rawInsn(_.visitInsn(ICONST_0))
      _ <- placeOnStack(left)
      _ <- box(left.tpe)
      _ <- rawInsn(_.visitInsn(AASTORE))
      _ <- dupe
      _ <- rawInsn(_.visitInsn(ICONST_1))
      _ <- placeOnStack(right)
      _ <- box(right.tpe)
      _ <- rawInsn(_.visitInsn(AASTORE))
    } yield ()
  }
}
