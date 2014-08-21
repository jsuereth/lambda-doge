package doge.compiler.types

import doge.compiler.ast

import scala.util.parsing.input.Position
import TypeSystem._
import Typer.Substitutions
import doge.compiler.ast._
import scalaz._
import Scalaz._

case class SyntaxTypeError(pos: Position, msg: String) extends Exception(
  s"$msg at\n${pos.longString}"
)

/** The state through which we thread the typer.
  *
  * See the companion objects for access to type state internals.
  */
case class TyperEnvironment(
  env: Env,
  substitutions: Substitutions
)
object TyperEnvironment {
  type TyperState[A] = State[TyperEnvironment, A]

  /** Grabs the current type environment (scope) for the typer. */
  def env: TyperState[Env] = State[TyperEnvironment, Env] { state => state -> state.env }
  /** Grabs the current known variable refinements in the current typer run. */
  def substitutions: TyperState[Substitutions] = State[TyperEnvironment, Substitutions] { state => state -> state.substitutions }
  /** Adds term->type associations to the environment in this typer. */
  def addEnvironment(types: (String, Type)*): TyperState[Unit] = State[TyperEnvironment, Unit] {
    case x: TyperEnvironment =>
      x.copy(env = x.env.withAdded(types:_*)) -> ()
  }
  /** Adds refinements (type-variable => type) to the typer state in this environment. */
  def addSubstitutions(refines: Substitutions): TyperState[Unit] = State[TyperEnvironment, Unit] { state =>
    (state.copy(substitutions = state.substitutions ++ refines), ())
  }
  /** Clears all used substitutions.  usually necessary between let expressions to limit total inference. */
  def clearSubstitutions: TyperState[Unit] =
    State[TyperEnvironment, Unit] { state =>
      (state.copy(substitutions = Map.empty[Long, Type]), ())
    }

  /** Places some value inside the TyperState Monad, allowing future computations to lookup
    * values in the state.
    */
  def withState[A](a: A): TyperState[A] = state[TyperEnvironment, A](a)
}
/**
 * A typer/inferencer for the Doge language.
 */
object Typer {
  import TyperEnvironment._
  type Substitutions = Map[Long, Type]

  // Note:  Use typeAst for stateful version.
  // This starts with state set to the passed in environment.
  def typeTree(ast: DogeAst, env: Env): TypedAst = {
    val typerRun =
      for {
        ast <- typeAst(ast)
        cleaned <- pruneAst(ast)
      } yield cleaned
    val (_, result) = typerRun(TyperEnvironment(env, Map.empty))
    result
  }


  // Note:  Use typeAst for stateful version.
  // This starts with state set to the passed in environment.
  def typeFull(m: Module, env: Env): ModuleTyped = {
    val typerRun =
      for {
        ast <- typeModule(m)
        cleaned <- pruneAst(ast)
      } yield cleaned.asInstanceOf[ModuleTyped] // TODO - lame hack
    val (_, result) = typerRun(TyperEnvironment(env, Map.empty))
    result
  }


  /** Will compute the type for a given AST. */
  def typeAst(ref: DogeAst): TyperState[TypedAst] =
    // Hacky casts for variance, yay dawg.  We could also just map(x => x).
    ref match {
      case id: IdReference => typeIdReference(id).asInstanceOf[TyperState[TypedAst]]
      case i: IntLiteral => typeIntLiteral(i).asInstanceOf[TyperState[TypedAst]]
      case b: BoolLiteral => typeBoolLiteral(b).asInstanceOf[TyperState[TypedAst]]
      case app: ApExpr => typeApply(app).asInstanceOf[TyperState[TypedAst]]
      case let: LetExpr => typeLet(let).asInstanceOf[TyperState[TypedAst]]
      case m: Module => typeModule(m).asInstanceOf[TyperState[TypedAst]]
    }

  /** Types a complete module definition, including sharing let expressions. */
  def typeModule(module: ast.Module): TyperState[ModuleTyped] = {
    def typeLetAndExpose(l: LetExpr): TyperState[LetExprTyped] =
       for {
         lt <- typeLet(l)
         pl <- pruneAst(lt)
         _ <- clearSubstitutions
         _ <- addEnvironment(lt.name -> pl.tpe)
       } yield pl.asInstanceOf[LetExprTyped] // TODO - not so hacky
    for {
      args <- module.definitions.toList.traverse[TyperState, LetExprTyped](typeLetAndExpose)
    } yield ModuleTyped(module.name, args)
  }


  /** Computes type of IntLiteral ASTs. */
  private def typeIntLiteral(ast: IntLiteral): TyperState[IntLiteralTyped] =
    State(x => x -> IntLiteralTyped(ast.value, ast.pos))

  private def typeBoolLiteral(ast: BoolLiteral): TyperState[BoolLiteralTyped] =
    State(x => x -> BoolLiteralTyped(ast.value, ast.pos))
  /** Computes the type of ID references.  Essentially just a lookup on state. */
  private def typeIdReference(ref: IdReference): TyperState[IdReferenceTyped] =
    for {
      id <- withState(ref)
      e <- env
    } yield IdReferenceTyped(id.name, e.lookup(id.name), ref.pos)

  /** Creates a new function type which is a curried
    * application of all types in the argument AST and a
    * variable result type, used for inference/typechecking of result type.
    */
  private def makeFuncForRefinement(argTypes: Seq[TypedAst]): Type = {
    val resultType = newVariable
    argTypes.foldRight[Type](resultType) { (prev, result) =>
      Function(prev.tpe, result)
    }
  }


  /**
   * Will type a lambda-application tree.
   */
  private def typeApply(ref: ApExpr): TyperState[ApExprTyped] =
    for {
      apExpr <- withState(ref)
      funTree <- typeIdReference(apExpr.name)
      argTypes <- apExpr.args.toList.traverse(typeAst)
        // TODO - better positions for errors here...
      result <- unify(makeFuncForRefinement(argTypes), funTree.tpe, ref.pos)
    } yield {
      // Rip result type out of unified type:
      val resultType = argTypes.foldLeft(result) {
        case (Function(from, to), current) => to
        case t => throw new SyntaxTypeError(ref.pos, s"Expected function type, got: $t while looking for result type of $result")
      }
      ApExprTyped(funTree, argTypes, resultType, ref.pos)
    }


  /**
   * Will type a let tree.  Does not add the let types into the state when complete.
   */
  private def typeLet(ref: LetExpr): TyperState[LetExprTyped] = {
    if(ref.argNames.isEmpty) {
      // Simple let is just a name assigned to an expression
      for {
        result <- typeAst(ref.definition)
      } yield LetExprTyped(ref.name, Nil, result, result.tpe, ref.pos)
    } else {
      // Complicated let is complicated.
      // Lambda is: name => (argNmaes curried) => definitioin type
      val argToType: Map[String, Type] =
        (ref.argNames.map(n => n -> newVariable))(collection.breakOut)
      // This constructs a functoin type using the variable argument types
      // and the resulting type of the expression.
      // Note: We attempt to prune variables after calling this.
      def makeFuncType(resultType: Type): Type = {
         ref.argNames.foldRight(resultType) { (argName, result) =>
           val argType = argToType(argName)
           Function(argType, result)
         }
      }
      for {
        _ <- addEnvironment(argToType.toSeq:_*)
        resultAst <- typeAst(ref.definition)
      } yield LetExprTyped(ref.name, ref.argNames, resultAst, makeFuncType(resultAst.tpe), ref.pos)
    }
  }

  /** Prunes type variables out of the ast using the typer state.
    *
    * NOTE: any time we recurse down like this, we're taking a perf hit.
    */
  private def pruneAst(ast: TypedAst): TyperState[TypedAst] = {
    def pruneRef(id: IdReferenceTyped): TyperState[IdReferenceTyped] = {
      for {
        tpe <- recursivePrune(id.tpe)
      } yield IdReferenceTyped(id.name, tpe, id.pos)
    }
    def pruneAp(ap: ApExprTyped): TyperState[ApExprTyped] = {
      for {
        tpe <- recursivePrune(ap.tpe)
        args <- ap.args.toList.traverse(pruneAst)
        name <- pruneRef(ap.name)
      } yield ApExprTyped(name, args, tpe, ap.pos)
    }
    def pruneLet(l: LetExprTyped): TyperState[LetExprTyped] = {
      for {
        tpe <- recursivePrune(l.tpe)
        defn <- pruneAst(l.definition)
      } yield LetExprTyped(l.name, l.argNames, defn, tpe, l.pos)

    }
    def pruneModule(l: ModuleTyped): TyperState[ModuleTyped] = {
      for {
        lets <- l.definitions.toList.traverse[TyperState, LetExprTyped](pruneLet)
      } yield ModuleTyped(l.name, lets)

    }
    ast match {
      case let: LetExprTyped => pruneLet(let).asInstanceOf[TyperState[TypedAst]]
      case ap: ApExprTyped => pruneAp(ap).asInstanceOf[TyperState[TypedAst]]
      case ref: IdReferenceTyped => pruneRef(ref).asInstanceOf[TyperState[TypedAst]]
      case l: LiteralTyped => withState(l)
      case m: ModuleTyped => pruneModule(m).asInstanceOf[TyperState[TypedAst]]
    }
  }

  /** Recursively replaces type variables with substitutions, following more than one chain if needed. */
  def prune(t: Type): TyperState[Type] =
    for {
      r <- substitutions
      result <- (t match {
        case v: TypeVariable if r.contains(v.id) => prune(r(v.id))
        case _ => withState(t)
      })
    } yield result

  /** Recursively descends a type, replacing type variables with substitutions. */
  def recursivePrune(t: Type): TyperState[Type] =
    t match {
      case TypeConstructor(name, args) =>
        for {
          targs <- args.toList.traverse[TyperState, Type](recursivePrune)
        } yield TypeConstructor(name, targs)
      case x => prune(t)
    }


  /** Fundamental unit of type infernce:
    *
    * Attempts to unify two types, such that any variables in t1 or t2 are replaced with known types in
    * the other.  Returns the resulting type.
    *
    * Throws a TypeError if unable to unify types due to a type checking conflict.
    * @param t1
    * @param t2
    * @return
    */
  def unify(t1: Type, t2: Type, pos: Position): TyperState[Type] = {
    /** A method which marks a type variable as being replacable with another type.
      *
      * This modifies the "state" of the typer, such that for the rest of this session,
      * anytime the variable `v` is seen, it can be typechecked as if `t` was seen.
      */
    def substitute(v: TypeVariable, t: Type) = State[TyperEnvironment, Type] { state =>
      if(v != t) {
        state.copy(substitutions = state.substitutions + (v.id -> t)) -> t
      } else state -> t
    }
    for {
      pt1 <- prune(t1)
      pt2 <- prune(t2)
      result <- (pt1, pt2) match {
        // Detected two type variables as equivalent.
        case (a: TypeVariable, b: TypeVariable) =>
          if (a.id < b.id) substitute(b, a)
          else substitute(a, b)
        // Detected that type variable a can use b's value.
        case (a: TypeVariable, b) =>
          if (a != b) {
            if (occursIn(a, b)) throw SyntaxTypeError(pos, s"recursive unification of $a and $b")
            // Everywhere A shows up we need to replace with B.
            substitute(a, b)
          } else withState(a)
        // Detect a type variable to unify with a constructor, so we invert the relationship and delegate.
        case (a: TypeConstructor, b: TypeVariable) => unify(b, a, pos)
        // Fundamental type-check operation.  We don't support polymorphism, type/term names need to
        // be exact, including for all args.
        case (a: TypeConstructor, b: TypeConstructor) =>
          // TODO - We should attempt to give a better error message here.
          if (a.name != b.name || a.args.length != b.args.length) throw SyntaxTypeError(pos, s"Type mismatch: $a != $b")
          // If all the args are the same, then we are the same type.
          for {
            targs <- a.args.zip(b.args).toList.traverse[TyperState, Type]({ case (l,r) => unify(l,r, pos)})
          } yield TypeConstructor(a.name, targs)
      }
    } yield result
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
}