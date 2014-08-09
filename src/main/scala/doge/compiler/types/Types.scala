package doge.compiler.types

class TypeError(msg: String) extends Exception(msg)

/**
 * A typer/inferencer for the Doge language.
 */
object Typer {
  import TypeSystem._
  import doge.compiler.ast._
  def typeTree(ast: DogeAst, env: Env): Type =
    typeTree(ast, env, Set.empty)

  def typeTree(ast: DogeAst, env: Env, nonGen: Set[Variable]): Type = {
    val (tpe, refinements) =
      typeTreeImpl(ast, env, nonGen, Map.empty)
    prune(tpe, refinements)
  }
  // TODO - Let expressions will alter the environment, but it is not done here.
  // This means recursive lets are not allowed...
  def typeTreeImpl(ast: DogeAst, env: Env, nonGen: Set[Variable], refinements: Refinements): (Type, Refinements) = ast match {
    case IdReference(term) => env.lookup(term) -> refinements
    // TODO - Other literals.
    case _: IntLiteral => TypeSystem.Integer -> refinements
    case ApExpr(ref, args) =>
      val (funtype, r1) = typeTreeImpl(ref, env, nonGen, refinements)
      val (methodType, r2) = typeTreeImpl(ref, env, nonGen, r1)
      val argTypes = args.map(ast => typeTree(ast, env, nonGen))
      // TODO - construct curried function types with arg types and unbound(variable) resultType.
      // Then unify with the type attached to the method.
      val resultType = newVariable
      val tmpApplyType = argTypes.foldRight[Type](resultType) { (prev, result) =>
        Function(prev, result)
      }
      val (result, r3) = unify(tmpApplyType, methodType)
      // TODO - return result call...
      argTypes.foldLeft(result) {
        case (Function(from, to), current) => to
        case t => throw new TypeError(s"Expected function type, got: $t while looking for result type of $result")
      } -> r3
    // TODO - Let is conflated with lambda
    case LetExpr(name, optTypeStrings, argNames, definition) =>
      val (result, rs) = if(!argNames.isEmpty) {
        // Lambda is: name => (argNames curried) => definition type.
        // TODO - we need to capture refinedments on this variable as we recursively typecheck,
        // so we get an acurate type out.
        val argToType: Map[String, Type] =
          (argNames.map(n => n -> TypeSystem.newVariable))(collection.breakOut)
        val (resultType, r1) = typeTreeImpl(definition, env.withAdded(argToType.toSeq:_*), nonGen, refinements)
        // TODO - we want to prune any variables we've found here.
        val result = argNames.foldLeft(resultType) { (result, argName) =>
          val argType = argToType(argName)
          Function(argType, result)
        }
        recursivePrune(result, r1) -> r1
      } else {
        // We just type the expression, this is not a function/thunk.
        typeTreeImpl(definition, env, nonGen, refinements)
      }
      result -> rs
  }
}


/** A type system in which ALL types are either
  * -  Applied operations
  * -  Variable
  *
  * Simple types are encoded as operations with no arguments.
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
  case class Variable(id: Int) extends Type {
    override def isMonotype = false
    override def isSimple = false
    override def toString = s"?$id"
  }

  /**
   * A type operator, i.e. One which applies a type function with types.
   *
   * Simple types are considered operators with no
    */
  case class Oper(name: String, args: Seq[Type]) extends Type {
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
    def apply(from: Type, to: Type): Oper = Oper("→", Array(from, to))
    def unapply(t: Type): Option[(Type,Type)] =
      t match {
        case Oper("→", Seq(from, to)) => Some(from -> to)
        case _ => None
      }
  }
  def FunctionN(to: Type, from: Type*): Type =
    from match {
      case Nil => to
      case Seq(next) => Function(next, to)
      case Seq(head, tail @ _*) => Function(head, FunctionN(to, tail:_*))
    }
  def Simple(name: String) = Oper(name, Nil)

  // Simple types are Type operators with no arguments.
  // Here we hardcode the types for the literals in Doge.
  val Integer = Simple("int")
  val Bool = Simple("bool")
  val String = Simple("String")
  val Unit = Simple("Unit")

  // Helpers to generate variable names on demand.
  private[this] var _nextVariableIdx = 0
  private def nextUniqueName = {
    val result = _nextVariableIdx
    _nextVariableIdx = _nextVariableIdx.toInt + 1
    "var-" + result.toString
  }
  // Helpers to generate unique variables
  private[this] var _nextVariableId = 0

  // Generates a new variable type.
  def newVariable: Variable = {
    val result = _nextVariableId
    _nextVariableId += 1
    Variable(result)
  }


  type Refinements = Map[Int,Type]

  def prune(t: Type, refinements: Refinements): Type =
    t match {
      case v: Variable if refinements.contains(v.id) =>
        prune(refinements(v.id), refinements)
      case _ => t
    }

  def recursivePrune(t: Type, refinements: Refinements): Type = t match {
    case Oper(name, args) => Oper(name, args.map(t => recursivePrune(t, refinements)))
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
    var refinements = Map.empty[Int, Type]
    def refine(v: Variable, t: Type): Type = if(v != t) {
      refinements += (v.id -> t)
      t
    } else t
    def prune(t: Type): Type = TypeSystem.this.prune(t, refinements)
    // Here we keep track of mutating variable types. This is hidden so we don't expose it in the rest of the type system.
    def unifyInternal(t1: Type, t2: Type): Type = {
      (prune(t1), prune(t2)) match {
        // For convenience, we also pick the lowest var when unifying.
        case (a: Variable, b: Variable) =>
          if (a.id < b.id) refine(b, a)
          else refine(a,b)
        // Here we check to see if a variable type on one side shows up
        // inside a type function the other side.
        case (a: Variable, b) =>
          if (a != b) {
            if (occursIn(a, b)) throw new TypeError(s"recursive unification of $a and $b")
            // Everywhere A shows up we need to replace with B.
            refine(a,b)
          } else a
        // We also put vars on the left to check.
        case (a: Oper, b: Variable) => unifyInternal(b, a)
        // Fundamental check operations.  We don't support polymorphism, type/term names need to
        // be exact.
        case (a: Oper, b: Oper) =>
          if (a.name != b.name ||
            a.args.length != b.args.length) throw new TypeError(s"Type mismatch: $a != $b")
          // If all the args are the same, then we are the same type.
          //a.args.zip(b.args).map({ case(l,r) => unify(l,r)})
          Oper(a.name, a.args.zip(b.args).map({ case (l, r) => unifyInternal(l, r)}))
      }
    }
    recursivePrune(unifyInternal(t1, t2), refinements) -> refinements
  }

  /** Returns true if a given type variable returns inside the other set of types. */
  private def occursIn(v: Variable, tpe: Type): Boolean = {
    tpe match {
      case `v` => true
      case Oper(name, args) => occursIn(v, args)
      case _ => false
    }
  }

  /** Returns true if a given type variable occurs inside the list of types. */
  private def occursIn(t: Variable, list: Seq[Type]): Boolean =
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
