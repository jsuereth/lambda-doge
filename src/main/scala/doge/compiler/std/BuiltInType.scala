package doge.compiler.std

import doge.compiler.backend.MethodWriterState
import doge.compiler.symbols.SymbolTable
import doge.compiler.types.TypeSystem.Type
import doge.compiler.types.TypedAst
import doge.compiler.types.TypeEnvironmentInfo
import org.objectweb.asm.signature.SignatureVisitor

import scalaz._


/** An interface for adding a built-in type to lambda Doge.
  *
  * Built-in types do *NOT* have classes associated with them.  All built-in types must rely on JDK core classes, or
  * be completely standalone implementation on top of primtiives (like bytes, booleans or arrays).
  *
  *
  */
trait BuiltInType {
  // TODO - This should be rmeoved for symbol tables.
  def typeTable: Seq[TypeEnvironmentInfo] = Nil

  /**
   * This is a method which knows how to generate the bytecode for built-in functions.  This method is
   * responsible for taking an AST of a built-in function call and generate the appropriate JVM bytecode to
   * execute the function.
   *
   * You do not need to worry about curried application, as built-in symbols are automatically lifted into
   * java methods as needed.
   */
  def backend: PartialFunction[TypedAst, State[MethodWriterState, Unit]] = PartialFunction.empty

  /**
   * A lame hack:  This allows us to visit the "raw" java signature (i.e. not the generic one) for our type.
   *
   * During JVM signature generation, this function will get checked to see if it can handle a given type
   * and call the appropriate "SignatureVisitor" methods.  For built-in-primitives, this should call
   * the appropriate type. For most other things, it should specific a java.lang.Object.
   */
  def visitSignatureInternal: PartialFunction[(SignatureVisitor, Type), Unit] = PartialFunction.empty

  /**
   * The symbols brough in by the built-in type.
   *
   * These will be included *after* all Java classpath symbols but before any user-defined symbols or imports.
   *
   * All symbols provided should use the [[doge.compiler.symbols.BuiltInSymbolTable]] and assocaited symbol constructors
   * which will mark all visible types/names as being built-in and requiring a custom backend to generate
   * necessary bytecode.
   *
   * Additionally any built-in method will get bridged if called in a curried fashion.
   * @return
   */
  def symbolTable: SymbolTable

  // TODO - Mapping for JDK type signatures + hook into getFunctionSignature(function: Type)
}
object BuiltInType {
  /** Joins two built in type interfaces into a new built-in type.  This allows us to use ONE built-in interface
    * for all built-in types in the entire language.
    */
  def join(l: BuiltInType, r: BuiltInType): BuiltInType = new BuiltInType {
    override val typeTable = (l.typeTable ++ r.typeTable)
    override val backend = (l.backend orElse r.backend)
    override val visitSignatureInternal = (l.visitSignatureInternal orElse r.visitSignatureInternal)
    override val symbolTable: SymbolTable =
      SymbolTable.join(l.symbolTable, r.symbolTable)
  }

  // FOR now, all built-in-support is right here.
  def all = Seq(DogeTuple2, Integers, Booleans, Lists).reduce(join)
}