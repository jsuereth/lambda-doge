package doge.compiler.std

import doge.compiler.backend.MethodWriterState
import doge.compiler.types.TypeSystem.{TypeConstructor, Type, newVariable, FunctionN, Function}
import doge.compiler.types.{IdReferenceTyped, ApExprTyped, TypeSystem, TypedAst}
import org.objectweb.asm.signature.SignatureVisitor
import org.objectweb.asm.Opcodes._

import scalaz._


object Lists extends BuiltInType {
  val name = "List"
  val NIL = "Nil"
  val CONS = "cons"
  val HEAD = "hd"
  val TAIL = "tl"

  override val typeTable: Map[String, Type] = Map(
    NIL -> TypeSystem.ListType,
    CONS -> {
      val a = newVariable
      val lst = TypeConstructor("List", Seq(a))
      FunctionN(lst, a, lst)
    },
    HEAD -> {
      val a = newVariable
      val lst = TypeConstructor("List", Seq(a))
      Function(lst, a)
    },
    TAIL -> {
      val a = newVariable
      val lst = TypeConstructor("List", Seq(a))
      Function(lst, lst)
    }
  )

  override val backend: PartialFunction[TypedAst, State[MethodWriterState, Unit]] = {
    case ApExprTyped(i, _, _, _) if i.name == NIL => writeNil
    case IdReferenceTyped(NIL, _, _)  => writeNil
    case ApExprTyped(i, Seq(front, lstExpr), tpe, _) if i.name == CONS => writeCons(front, lstExpr)
    case ApExprTyped(i, Seq(lstExpr), tpe, _) if i.name == HEAD => writeHead(lstExpr)
    case ApExprTyped(i, Seq(lstExpr), tpe, _) if i.name == TAIL => writeTail(lstExpr)
  }

  import doge.compiler.backend.MethodWriter._

  private val writeNil = State[MethodWriterState, Unit] { state =>
    // TODO - Re-use the same instance rather than copying...
    state.mv.visitTypeInsn(NEW, "java/util/concurrent/CopyOnWriteArrayList")
    state.mv.visitInsn(DUP)
    state.mv.visitMethodInsn(INVOKESPECIAL, "java/util/concurrent/CopyOnWriteArrayList", "<init>", "()V")
    state -> ()
  }

  private def writeCons(front: TypedAst, list: TypedAst): State[MethodWriterState, Unit] = {
    for {
      _ <- placeOnStack(list)
      _ <- dupe  // We have to dupe because the append method returns void, and we need a result.
      _ <- rawInsn(_.visitInsn(ICONST_0))
      _ <- placeOnStack(front)
      _ <- box(front.tpe)
      _ <- rawInsn(_.visitMethodInsn(INVOKEINTERFACE, "java/util/List", "add", "(ILjava/lang/Object;)V"))
    } yield ()
  }
  private def writeHead(list: TypedAst): State[MethodWriterState, Unit] = {
    for {
      _ <- placeOnStack(list)
      _ <- rawInsn { mv =>
        mv.visitInsn(ICONST_0)
        mv.visitMethodInsn(INVOKEINTERFACE, "java/util/List", "get", "(I)Ljava/lang/Object;")
      }
      _ <- unbox(listElementType(list.tpe))
    } yield ()
  }
  private def writeTail(list: TypedAst): State[MethodWriterState, Unit] = {
    for {
      _ <- placeOnStack(list)
      _ <- dupe
      _ <- rawInsn { mv =>
        mv.visitInsn(ICONST_0)
        mv.visitMethodInsn(INVOKEINTERFACE, "java/util/List", "remove", "(I)Ljava/lang/Object;")
        // ignore the boolean return
        mv.visitInsn(POP)
      }
    } yield ()
  }
  override val visitSignatureInternal: PartialFunction[(SignatureVisitor, Type), Unit] = {
    case (sv, TypeConstructor("List", Seq(elType))) =>
      // TODO - figure out generics
      sv.visitClassType("java/util/List;")
  }

  private def listElementType(tpe: Type): Type =
    tpe match {
      case TypeConstructor("List", Seq(el)) => el
      case _ => sys.error(s"$tpe is not a valid list type.")
    }
}
