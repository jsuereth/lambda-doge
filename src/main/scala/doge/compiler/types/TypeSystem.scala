package doge.compiler.types


case class TypeError(msg: String) extends Exception(msg)
/** A type system in which ALL types are either
  * -  Type Constructors
  * -  Variable
  *
  * Simple types are encoded as type constructors with no arguments.
  *
  * This aims to be a BARE MINIMUM representation of types which is eminently testable.
  * We try to avoid any complex interaction with the AST here, but we DO try to provide
  * necessary type unification in a testable manner.
  *
  * Concerns:
  *   - Type variable identity is flaky, at best.
  */
object TypeSystem {

  // TODO - We need a way to pickle types in/out for modules...

  /** Base class for all types in this type system. */
  sealed abstract class Type {
    def isMonotype: Boolean
    def isSimple: Boolean
  }
  /** A free type variable, unbounded (e.g. "for all types a").
    * We track these by id, in case they show up in multiple aspects of a type.
    */
  case class TypeVariable(id: Long) extends Type {
    override def isMonotype = false
    override def isSimple = false
    override def toString = s"?$id"
  }

  /**
   * A type operator, i.e. One which applies a type function with types.
   *
   * Simple types are considered operators with no arguments.
   *
   * In terms of kind, it's based on the # of arguments remaining that are type variables
   *    0 -  *
   *    1 - * -> *
   *    2 - * -> * -> *
   *    etc.
   * EXCEPT, we can't really implement an effective kind system in this fashion.
   */
  case class TypeConstructor(name: String, args: Seq[Type]) extends Type {
    override def isMonotype: Boolean =
      args.isEmpty || args.forall(_.isMonotype)
    override def isSimple = args.isEmpty

    // TODO - this shouldn't be hardcoded...
    override def toString =
      if(args.isEmpty) s"$name"
      else if(name == "Tuple2") s"(${args(0)}, ${args(1)})"
      else if(name == "→") s"${args(0)} $name ${args(1)}"
      else s"($name ${args.mkString(" ")})"
  }

  /** helper to create a function type of From -> To. */
  object Function {
    def apply(from: Type, to: Type): TypeConstructor = TypeConstructor("→", Array(from, to))
    def unapply(t: Type): Option[(Type,Type)] =
      t match {
        case TypeConstructor("→", Seq(from, to)) => Some(from -> to)
        case _ => None
      }
    // TODO - Handle failure
    def arity(t: Type): Int = t match {
      case Function(l, r) => 1 + arity(r)
      case _ => 0
    }
  }
  def FunctionN(to: Type, from: Type*): Type =
    from match {
      case Nil => to
      case Seq(next) => Function(next, to)
      case Seq(head, tail @ _*) => Function(head, FunctionN(to, tail:_*))
    }
  def Simple(name: String) = TypeConstructor(name, Nil)

  // Helpers to generate unique variables
  private[this] val _nextVariableId = new java.util.concurrent.atomic.AtomicLong(0)

  // Generates a new variable type.  While this mutates a global id field, it's
  // meant to be referentially transparent, in creating a unique type variable that
  // is not the same as another.  The id itself should not matter for
  // correct execution.
  def newVariable: TypeVariable = {
    TypeVariable(_nextVariableId.getAndIncrement)
  }



  // Simple types are Type operators with no arguments.
  // Here we hardcode the types for the literals in Doge.
  val Integer = Simple("int")
  val Bool = Simple("bool")

  // TODO - Unsupported types
  val String = Simple("String")  // TODO - Alias this to list int?
  val Unit = Simple("Unit")
  val ListType = TypeConstructor("List", Seq(newVariable))
  val MapType = TypeConstructor("Map", Seq(newVariable, newVariable))
  val Tuple2Type = TypeConstructor("Tuple2", Seq(newVariable, newVariable))
}

