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

  /** Defines an environment where we can lookup types.
    *  e.g. when type checking an expression, we need to
    *  know the types of terms/references used.
    */
  trait Env {
    def lookup(name: String): Type
    def withAdded(types: (String, Type)*): Env
  }

  def dumbEnvironment(initial: Map[String, Type] = Map.empty): Env = new Env {
    def lookup(name: String): Type = {
      initial.get(name).getOrElse {
        throw new TypeError(s"Type not found for term: $name in ${initial.mkString("\n * ","\n * ", "\n")}")
      }
    }
    def withAdded(types: (String, Type)*): Env = {
      dumbEnvironment(initial ++ types)
    }
  }

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
   */
  case class TypeConstructor(name: String, args: Seq[Type]) extends Type {
    override def isMonotype: Boolean =
      args.isEmpty || args.forall(_.isMonotype)
    override def isSimple = args.isEmpty

    override def toString =
      if(args.isEmpty) s"$name"
      else if(args.length == 2) s"${args(0)} $name ${args(1)}"
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
  }
  def FunctionN(to: Type, from: Type*): Type =
    from match {
      case Nil => to
      case Seq(next) => Function(next, to)
      case Seq(head, tail @ _*) => Function(head, FunctionN(to, tail:_*))
    }
  def Simple(name: String) = TypeConstructor(name, Nil)

  // Simple types are Type operators with no arguments.
  // Here we hardcode the types for the literals in Doge.
  val Integer = Simple("int")
  val Bool = Simple("bool")
  val String = Simple("String")
  val Unit = Simple("Unit")


  // Helpers to generate unique variables
  private[this] val _nextVariableId = new java.util.concurrent.atomic.AtomicLong(0)

  // Generates a new variable type.
  def newVariable: TypeVariable = TypeVariable(_nextVariableId.getAndIncrement)


  type Refinements = Map[Long, Type]

  def prune(t: Type, refinements: Refinements): Type =
    t match {
      case v: TypeVariable if refinements.contains(v.id) =>
        prune(refinements(v.id), refinements)
      case _ => t
    }

  def recursivePrune(t: Type, refinements: Refinements): Type = t match {
    case TypeConstructor(name, args) => TypeConstructor(name, args.map(t => recursivePrune(t, refinements)))
    case x => prune(x, refinements)
  }

  /**
   * A fundamental unit of type checking. This will take two types and attempt to unify
   * concrete types + type variables between the two so that all type variables
   * which could be known are inhabited with known types.
   *
   * Note Mutates the types.   Attempts to unify one type with another.
   * All variable types will be given instances (if applicable).
   * Throws TypeErrors if it finds:
   * - A recursive type is discovered.
   * - We attempt to unify two concrete types which are actually different.
   *
   *
   * Note2: This is operation order dependent on type variables which get picked in the
   *        unified type.  Not sure if this has disastrous resuls or not.
   *
   * @return The resulting unified type, or NULL if we are unable to unify.
   */
  def unify(t1: Type, t2: Type): (Type, Refinements) = {
    var refinements = Map.empty[Long, Type]
    def refine(v: TypeVariable, t: Type): Type = if(v != t) {
      refinements += (v.id -> t)
      t
    } else t
    def prune(t: Type): Type = TypeSystem.this.prune(t, refinements)
    // Here we keep track of mutating variable types. This is hidden so we don't expose it in the rest of the type system.
    def unifyInternal(t1: Type, t2: Type): Type = {
      (prune(t1), prune(t2)) match {
        // For convenience, we also pick the lowest var when unifying.
        case (a: TypeVariable, b: TypeVariable) =>
          if (a.id < b.id) refine(b, a)
          else refine(a,b)
        // Here we check to see if a variable type on one side shows up
        // inside a type function the other side.
        case (a: TypeVariable, b) =>
          if (a != b) {
            if (occursIn(a, b)) throw TypeError(s"recursive unification of $a and $b")
            // Everywhere A shows up we need to replace with B.
            refine(a,b)
          } else a
        // We also put vars on the left to check.
        case (a: TypeConstructor, b: TypeVariable) => unifyInternal(b, a)
        // Fundamental check operations.  We don't support polymorphism, type/term names need to
        // be exact.
        case (a: TypeConstructor, b: TypeConstructor) =>
          if (a.name != b.name ||
            a.args.length != b.args.length) throw TypeError(s"Type mismatch: $a != $b")
          // If all the args are the same, then we are the same type.
          //a.args.zip(b.args).map({ case(l,r) => unify(l,r)})
          TypeConstructor(a.name, a.args.zip(b.args).map({ case (l, r) => unifyInternal(l, r)}))
      }
    }
    // Here, we actually do the unification, then recursively prune refinements AGAIN, since we like
    // decending the type tree.
    val result = unifyInternal(t1, t2)
    recursivePrune(unifyInternal(t1, t2), refinements) -> refinements
  }

  /** Returns true if a given type variable returns inside the other set of types. */
  private def occursIn(v: TypeVariable, tpe: Type): Boolean = {
    tpe match {
      case `v` => true
      case TypeConstructor(name, args) => occursIn(v, args)
      case _ => false
    }
  }

  /** Returns true if a given type variable occurs inside the list of types. */
  private def occursIn(t: TypeVariable, list: Seq[Type]): Boolean =
    list exists (t2 => occursIn(t, t2))


  def main(args: Array[String]): Unit = {
    val idType = {
      val tmp = newVariable
      Function(tmp, tmp)
    }
    val functionCall = Function(newVariable, Integer)
    System.err.println(s"unify($idType, $functionCall) = ${unify(idType, functionCall)}")
  }

}

