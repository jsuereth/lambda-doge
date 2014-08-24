package doge.compiler.types

import TypeSystem.Type
/** Defines an environment where we can lookup types.
  *  e.g. when type checking an expression, we need to
  *  know the types of terms/references used.
  */
trait TypeEnv {
  def lookup(name: String): TypeEnvironmentInfo
  def withLocal(types: TypeEnvironmentInfo*): TypeEnv
}

/** An enumeration for possible locations/encodings of named expressions.
  *
  * This is where the JVM target blends with the raw compiler.  e.g.
  * here we know if a method/value is going to be inlined bytecode,
  * a static method dispatch, a virtual method dispatch or an
  * interface dispatch.
  */
sealed trait Location
object Location {
  def unapply(in: TypeEnvironmentInfo): Option[Location] = Some(in.location)
}
case object Argument extends Location {
  override def toString = "arg"
}
// Note: here we overlap JVM target + type system.
// This lets us know the JVM encoding of a particular method
case class StaticMethod(className: String, method: String, args: Seq[Type], result: Type) extends Location {
  def asFunctionType = TypeSystem.FunctionN(result, args:_*)
  override def toString = s"$className.$method"
}
case object BuiltIn extends Location {
  override def toString = "built-in"
}


// case class RemoteModule(className: String) extends Location
// TODO - JDK references

/** Represents environmental information about a type in some context.
  * @param name  The name that can be looked up.
  * @param location The location of the local type.
  * @param tpe   The type associated with the name.
  */
case class TypeEnvironmentInfo(name: String, location: Location, tpe: Type)

object TypeEnv {
  /** A very dumb environment with only the initial types passed in. */
  def dumbEnvironment(initial: Seq[TypeEnvironmentInfo] = Nil): TypeEnv = new TypeEnv {
    lazy val lookUp: Map[String, TypeEnvironmentInfo] = initial.groupBy(_.name).mapValues(_.head)
    override def lookup(name: String): TypeEnvironmentInfo = {
      lookUp.get(name).getOrElse {
        throw new TypeError(s"Type not found for term: $name in ${initial.mkString("\n * ", "\n * ", "\n")}")
      }
    }
    override def withLocal(types: TypeEnvironmentInfo*): TypeEnv = {
      dumbEnvironment(initial ++ types)
    }
  }
}
