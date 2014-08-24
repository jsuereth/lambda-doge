package doge.compiler.std

import doge.compiler.backend.MethodWriterState
import doge.compiler.types.TypeSystem.Type
import doge.compiler.types.TypedAst
import doge.compiler.types.TypeEnvironmentInfo
import org.objectweb.asm.signature.SignatureVisitor

import scalaz._



trait BuiltInType {
  // TODO - figure out how to import these rather than always included.
  def typeTable: Seq[TypeEnvironmentInfo] = Nil
  def backend: PartialFunction[TypedAst, State[MethodWriterState, Unit]] = PartialFunction.empty
  def visitSignatureInternal: PartialFunction[(SignatureVisitor, Type), Unit] = PartialFunction.empty

  // TODO - Mapping for JDK type signatures + hook into getFunctionSignature(function: Type)
}
object BuiltInType {
  def join(l: BuiltInType, r: BuiltInType): BuiltInType = new BuiltInType {
    override val typeTable = (l.typeTable ++ r.typeTable)
    override val backend = (l.backend orElse r.backend)
    override val visitSignatureInternal = (l.visitSignatureInternal orElse r.visitSignatureInternal)
  }

  // FOR now, all built-in-support is right here.
  def all = Seq(DogeTuple2, Integers, Booleans, Lists).reduce(join)
}