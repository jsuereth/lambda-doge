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
object  TypeSystem {

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
      else if(name == TUPLE2_TCONS_NAME) s"(${args(0)}, ${args(1)})"
      else if(name == FUNCTION_TCONS_NAME) s"${args(0)} $name ${args(1)}"
      else s"($name ${args.mkString(" ")})"
  }


  /** A predicate on a type.  We use this to denote OO inheritance, as well
    * as our own form of polymorphism via type-traits.
    */
  sealed trait TypePredicate

  /**
   * This predicate implies that a given type must support a given java interface or class.
   *
   * Note that this does not imply an "isA" relationship, as it could be a "hasA".
   * @param fullClassName
   */
  final case class IsIn(fullClassName: String) extends TypePredicate {
    override def toString = s"$fullClassName"
  }

  /**
   * A qualified type.   This is some "raw" type that is qualified by some sort of external restrictions.
   *
   * For example, the predicate may qualify that the type passed MUST have some external interface, or extend
   * some java class.
   */
  case class QualifiedType(pred: TypePredicate, underlying: Type) extends Type {
    override def isMonotype: Boolean = underlying.isMonotype
    override def isSimple = false
    override def toString = s"($pred $underlying)"
  }



  val FUNCTION_TCONS_NAME = "→"
  val TUPLE2_TCONS_NAME="Tuple2"

  /** helper to create a function type of From -> To. */
  object Function {
    def apply(from: Type, to: Type): TypeConstructor = TypeConstructor(FUNCTION_TCONS_NAME, Array(from, to))
    def unapply(t: Type): Option[(Type,Type)] =
      t match {
        case TypeConstructor(FUNCTION_TCONS_NAME, Seq(from, to)) => Some(from -> to)
        case _ => None
      }
    // TODO - Handle failure
    def arity(t: Type): Int = t match {
      case Function(l, r) => 1 + arity(r)
      case _ => 0
    }

    // Similar to above, but with a limited amount of deconstruction.
    def deconstructArgs(tpe: Type)(args: Int = arity(tpe)): (Seq[Type], Type) = {
      def deconstructArgs(argList: Seq[Type], nextFunc: Type, remaining: Int): Seq[Type] = nextFunc match {
        case Function(arg, next) if remaining > 0 => deconstructArgs(argList :+ arg, next, remaining -1)
        case result => argList :+ result
      }
      val all = deconstructArgs(Nil, tpe, args)
      (all.init, all.last)
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
  val Integer = Simple("Int")
  val Bool = Simple("Boolean")

  // TODO - Unsupported types
  val String = Simple("String")  // TODO - Alias this to list int?
  val Unit = Simple("Unit")
  val ListType = TypeConstructor("List", Seq(newVariable))
  val MapType = TypeConstructor("Map", Seq(newVariable, newVariable))
  val Tuple2Type = TypeConstructor(TUPLE2_TCONS_NAME, Seq(newVariable, newVariable))
}

