package doge.compiler.std

import doge.compiler.backend.MethodWriterState
import doge.compiler.types.TypeSystem.Type
import doge.compiler.types.TypedAst
import org.objectweb.asm.signature.SignatureVisitor

import scalaz._



trait BuiltInType {
  // TODO - figure out how to import these rather than always included.
  def typeTable: Map[String, Type]
  def backend: PartialFunction[TypedAst, State[MethodWriterState, Unit]]

  def visitSignatureInternal: PartialFunction[(SignatureVisitor, Type), Unit]

  // TODO - Mapping for JDK type signatures + hook into getFunctionSignature(function: Type)
}
object BuiltInType {
  def join(l: BuiltInType, r: BuiltInType): BuiltInType = new BuiltInType {
    override val typeTable = (l.typeTable ++ r.typeTable)
    override val backend = (l.backend orElse r.backend)
    override val visitSignatureInternal = (l.visitSignatureInternal orElse r.visitSignatureInternal)
  }

  // FOR now, all built-in-support is right here.
  def all = Seq(DogeTuple2, Integers, Booleans).reduce(join)
}