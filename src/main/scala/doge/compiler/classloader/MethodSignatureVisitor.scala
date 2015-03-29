package doge.compiler.classloader

import doge.compiler.types.TypeSystem
import doge.compiler.types.TypeSystem.Type
import org.objectweb.asm.Opcodes
import org.objectweb.asm.signature.SignatureVisitor

import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}

case class MethodSymbol(tpe: Type, arity: Int)

/**
 * This class can visit method signatures and return details.
 *
 *
 *  ( visitFormalTypeParameter visitClassBound? visitInterfaceBound* )* ( visitParameterType* visitReturnType visitExceptionType* )
 */
class MethodSignatureVisitor extends SignatureVisitor(Opcodes.ASM5) {
  private val symPromised: Promise[MethodSymbol] = Promise.apply[MethodSymbol]()
  private var arity: Int = 0
  private var args: Seq[Type] = Nil
  def sym: Future[MethodSymbol] = symPromised.future
  override def visitFormalTypeParameter(name: String): Unit = {
    symPromised.tryFailure(new IllegalStateException(s"Unable to handle java class generics!  Found $name"))
  }
  override def visitClassBound(): SignatureVisitor = {
    symPromised.tryFailure(new IllegalStateException(s"Unable to handle java class generics! (class bound)"))
    null
  }
  override def visitInterfaceBound(): SignatureVisitor = {
    symPromised.tryFailure(new IllegalStateException(s"Unable to handle java class generics! (interface bound)"))
    null
  }
  override def visitParameterType(): SignatureVisitor = {
    arity += 1
    val tv = new TypeSignatureVisitor
    tv.tpe.onComplete {
      case Success(tpe) => args = args :+ tpe
      case Failure(t) => symPromised.tryFailure(t)
    }(SameThreadExecutionContext)
    tv
  }
  override def visitReturnType(): SignatureVisitor ={
    val tv = new TypeSignatureVisitor
    tv.tpe.onComplete {
      case Success(tpe) =>
        val ftpe =
          if(args.isEmpty) tpe
          else TypeSystem.FunctionN(tpe, args:_*)
        val sym = MethodSymbol(ftpe, arity)
        symPromised.trySuccess(sym)
      case Failure(t) => symPromised.tryFailure(t)
    }(SameThreadExecutionContext)
    tv
  }
  // We specifically ignore exceptions
}
