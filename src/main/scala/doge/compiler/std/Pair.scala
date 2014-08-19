package doge.compiler
package std


import org.objectweb.asm.Opcodes._
import doge.compiler.types.{IdReferenceTyped, ApExprTyped, TypedAst}
import types.TypeSystem._
import backend._
import scalaz._
import Scalaz._

trait BuiltInType {
  // TODO - figure out how to handle terms and java scoping...
  def name: String
  def typeTable: Map[String, Type]
  def backend: PartialFunction[TypedAst, State[MethodWriterState, Unit]]

  // TODO - Mapping for JDK type signatures + hook into getFunctionSignature(function: Type)
}


/** Built in Tuple2 types/namespace. */
object DogeTuple2 extends BuiltInType {
  val name = "Tuple2"
  val typeTable = Map[String, Type](
    "fst" -> {
      val a = newVariable
      val b = newVariable
      Function(TypeConstructor("Tuple2", Seq(a, b)), a)
    },
    "snd" -> {
      val a = newVariable
      val b = newVariable
      Function(TypeConstructor("Tuple2", Seq(a, b)), b)
    },
    "tuple2" -> {
      val a = newVariable
      val b = newVariable
      FunctionN(TypeConstructor("Tuple2", Seq(a, b)), a, b)
    }
  )

  val backend: PartialFunction[TypedAst, State[MethodWriterState, Unit]] = {
    case ApExprTyped(IdReferenceTyped("tuple2", _), Seq(left, right), tpe) => constructorImpl(left, right)
    case ApExprTyped(IdReferenceTyped("fst", _), Seq(tuple), tpe) => fstMethodImpl(tuple)
    case ApExprTyped(IdReferenceTyped("snd", _), Seq(tuple), tpe) => fstMethodImpl(tuple)
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
      case TypeConstructor("Tuple2", Seq(left, right)) => left
      case _ => sys.error(s"$tpe is not a tuple!")
    }
  private def sndTupleType(tpe: Type): Type =
    tpe match {
      case TypeConstructor("Tuple2", Seq(left, right)) => right
      case _ => sys.error(s"$tpe is not a tuple!")
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
