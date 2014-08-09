package doge.compiler.types

import scala.util.parsing.input.Position
import TypeSystem._
import doge.compiler.ast._
import scalaz._
import Scalaz._

case class SyntaxTypeError(pos: Position, msg: String) extends Exception(
  s"\n${pos.longString} $msg"
)

/** The state through which we thread the typer.
  *
  * See the companion objects for access to type state internals.
  */
case class TyperEnvironment(
  env: Env,
  refinements: Refinements
)
object TyperEnvironment {
  type TyperState[A] = State[TyperEnvironment, A]

  /** Grabs the current type environment (scope) for the typer. */
  def env: TyperState[Env] = State[TyperEnvironment, Env] { state => state -> state.env }
  /** Grabs the current known variable refinements in the current typer run. */
  def refinements: TyperState[Refinements] = State[TyperEnvironment, Refinements] { state => state -> state.refinements }
  /** Adds term->type associations to the environment in this typer. */
  def addEnvironment(types: (String, Type)*): TyperState[Unit] = State[TyperEnvironment, Unit] {
    case x: TyperEnvironment =>
      x.copy(env = x.env.withAdded(types:_*)) -> ()
  }
  /** Adds refinements (type-variable => type) to the typer state in this environment. */
  def addRefinements(refines: Refinements): TyperState[Unit] = State[TyperEnvironment, Unit] { state =>
    (state.copy(refinements = state.refinements ++ refines), ())
  }
  // TODO - causes scala compiler crash, but not needed so far.
  //def clearRefinements: TyperState[Unit] = State[TyperEnvironment, Unit](x => x.copy(refinements = Map.empty) -> ())

  /** Places some value inside the TyperState Monad, allowing future computations to lookup
    * values in the state.
    */
  def withState[A](a: A): TyperState[A] = State[TyperEnvironment, A]({ s => (s,a)})
}
/**
 * A typer/inferencer for the Doge language.
 */
object Typer {
  import TyperEnvironment._

  // Note:  Use typeAst for stateful version.
  // This starts with state set to the passed in environment.
  def typeTree(ast: DogeAst, env: Env): TypedAst = {
    //typeTree(ast, env, Set.empty)
    val (state, result) = typeAst(ast)(TyperEnvironment(env, Map.empty))
    result
  }


  /** Will compute the type for a given AST. */
  def typeAst(ref: DogeAst): TyperState[TypedAst] =
    // Hacky casts for variance, yay dawg.  We could also just map(x => x).
    ref match {
      case id: IdReference => typeIdReference(id).asInstanceOf[TyperState[TypedAst]]
      case i: IntLiteral => typeIntLiteral(i).asInstanceOf[TyperState[TypedAst]]
      case app: ApExpr => typeApply(app).asInstanceOf[TyperState[TypedAst]]
      case let: LetExpr => typeLet(let).asInstanceOf[TyperState[TypedAst]]
    }

  /** Computes type of IntLiteral ASTs. */
  private def typeIntLiteral(ast: IntLiteral): TyperState[IntLiteralTyped] =
    State(x => x -> IntLiteralTyped(ast.value))
  /** Computes the type of ID references.  Essentially just a lookup on state. */
  private def typeIdReference(ref: IdReference): TyperState[IdReferenceTyped] =
    for {
      id <- withState(ref)
      e <- env
    } yield IdReferenceTyped(id.name, e.lookup(id.name))

  /** Helper method which will unify two types AND preserver
    * any discovered refinements inside the typer state.
    */
  private def preservingUnify(t: Type, t2: Type): TyperState[Type] = {
    val (result, newRefines) = unify(t, t2)
    for {
      _ <- addRefinements(newRefines)
    } yield result
  }

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

  /** Will prune type variables out of a type, using the
    * refinements associated with the current state.
    */
  private def statedRecursivePrune(t: Type): TyperState[Type] =
    for {
      r <- refinements
    } yield recursivePrune(t, r)

  /**
   * Will type a lambda-application tree.
   */
  private def typeApply(ref: ApExpr): TyperState[ApExprTyped] =
    for {
      apExpr <- withState(ref)
      funTree <- typeIdReference(apExpr.name)
      argTypes <- apExpr.args.toList.traverse(typeAst)
      result <- preservingUnify(makeFuncForRefinement(argTypes), funTree.tpe)
    } yield {
      // Rip result type out of unified type:
      val resultType = argTypes.foldLeft(result) {
        case (Function(from, to), current) => to
        case t => throw new TypeError(s"Expected function type, got: $t while looking for result type of $result")
      }
      ApExprTyped(funTree, argTypes, resultType)
    }


  /**
   * Will type a let tree.  Does not add the let types into the state when complete.
   */
  private def typeLet(ref: LetExpr): TyperState[TypedAst] = {
    if(ref.argNames.isEmpty) {
      // Simple let is just a name assigned to an expression
      for {
        result <- typeAst(ref.definition)
      } yield LetExprTyped(ref.name, Nil, result, result.tpe)
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
        // Here, after doing "global let inference" we prune all type variables out of the tree.
        ast <- pruneAst(LetExprTyped(ref.name, ref.argNames, resultAst, makeFuncType(resultAst.tpe)))
      } yield ast
    }
  }

  /** Prunes type variables out of the ast using the typer state. */
  private def pruneAst(ast: TypedAst): TyperState[TypedAst] = {
    def pruneRef(id: IdReferenceTyped): TyperState[IdReferenceTyped] = {
      for {
        tpe <- statedRecursivePrune(id.tpe)
      } yield IdReferenceTyped(id.name, tpe)
    }
    def pruneAp(ap: ApExprTyped): TyperState[ApExprTyped] = {
      for {
        tpe <- statedRecursivePrune(ap.tpe)
        args <- ap.args.toList.traverse(pruneAst)
        name <- pruneRef(ap.name)
      } yield ApExprTyped(name, args, tpe)
    }
    def pruneLet(l: LetExprTyped): TyperState[LetExprTyped] = {
      for {
        tpe <- statedRecursivePrune(l.tpe)
        defn <- pruneAst(l.definition)
      } yield LetExprTyped(l.name, l.argNames, defn, tpe)

    }
    ast match {
      case let: LetExprTyped => pruneLet(let).asInstanceOf[TyperState[TypedAst]]
      case ap: ApExprTyped => pruneAp(ap).asInstanceOf[TyperState[TypedAst]]
      case ref: IdReferenceTyped => pruneRef(ref).asInstanceOf[TyperState[TypedAst]]
      case l: LiteralTyped => withState(l)
    }
  }
}