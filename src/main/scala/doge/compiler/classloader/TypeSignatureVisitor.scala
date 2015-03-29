package doge.compiler.classloader

import doge.compiler.types.TypeSystem
import doge.compiler.types.TypeSystem.Type
import org.objectweb.asm.Opcodes
import org.objectweb.asm.signature.SignatureVisitor

import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}

/**
 * A class which will parse type signatures and return them in a future (when the parsing is completed).
 *
 *
 * Note: this class is synchronous, all futures are immediately evaluated.
 */
// Signature visitor for the following cases:
// visitBaseType |
// visitTypeVariable |
// visitArrayType |
// ( visitClassType visitTypeArgument* ( visitInnerClassType visitTypeArgument* )* visitEnd ) )
class TypeSignatureVisitor extends SignatureVisitor(Opcodes.ASM5) {
  private val tpePromised: Promise[Type] = Promise.apply[Type]()
  def tpe: Future[Type] = tpePromised.future
  override def visitBaseType(descriptor : Char): Unit = {
    val tpe=
      descriptor match {
        case 'I' => TypeSystem.Integer
        case 'V' => TypeSystem.Unit
        case 'Z' => TypeSystem.Bool
        // TODO - The rest of these built in
        case 'B' => TypeSystem.Simple("Byte")
        case 'C' => TypeSystem.Simple("Char")
        case 'D' => TypeSystem.Simple("Double")
        case 'F' => TypeSystem.Simple("Float")
        case 'J' => TypeSystem.Simple("Long")
        case 'S' => TypeSystem.Simple("Short")
        case _ => TypeSystem.TypeConstructor(descriptor.toString, Nil)
      }
    tpePromised.success(tpe)
  }
  override def visitTypeVariable(name: String): Unit = {
    tpePromised.failure(new IllegalStateException(s"Class generics not supported, found generic variable: $name"))
  }
  // The nested signature visitor will generate an underlying type.
  override def visitArrayType(): SignatureVisitor = {
    val nested = new TypeSignatureVisitor
    nested.tpe.onComplete {
      case Success(tpe) => tpePromised.trySuccess(TypeSystem.ArrayType(tpe))
      case Failure(t) => tpePromised.tryFailure(t)
    }(SameThreadExecutionContext)
    nested
  }

  // Here, we hide the state/ugliness of dealing with classes w/ type parameters.
  var typeArgs: Seq[Type] = Nil
  var className: Option[String] = None

  // Note: This is tricksy, as we can't change "this", but we'll be parsing type arguments, potentially.
  // visitClassType visitTypeArgument* ( visitInnerClassType visitTypeArgument* )* visitEnd
  override def visitClassType(name: String): Unit = {
    // This puts us in a state to handle possible higher kinded types.
    // TODO - JVM -> Java name conversion should be readily availble in utility method.
    className = Some(name.replaceAllLiterally("/", "."))
  }
  override def visitTypeArgument(): Unit = {
    typeArgs = typeArgs :+ TypeSystem.newVariable
  }
  override def visitTypeArgument(wildcard: Char): SignatureVisitor = {
    wildcard match {
      case SignatureVisitor.EXTENDS | SignatureVisitor.INSTANCEOF =>
      case SignatureVisitor.SUPER => tpePromised.tryFailure(new IllegalStateException(s"Unable to handle ? extends Super types."))
    }
    val visitor = new TypeSignatureVisitor
    visitor.tpe.onComplete {
      case Success(tpe) => typeArgs = typeArgs :+ tpe
      case Failure(t) => tpePromised.tryFailure(t)
    }(SameThreadExecutionContext)
    visitor
  }

  override def visitEnd(): Unit = {
    // If we're in class type mode, we finalize the type.
    className match {
      case Some(n) =>
        // TODO - We actually may still want to have "n" be a qualified type, even the typeArgs may need to be
        // qualified types...  Right now we're basically turning off the JVM's notion of variance or subclasses.
        // We *could* expose some kind of function back into place that coerces types...
        tpePromised.trySuccess(TypeSystem.TypeConstructor(n, typeArgs))
      case None =>
        // Ignore, but maybe issue warning that we're in the wrong state.
    }
  }
}
