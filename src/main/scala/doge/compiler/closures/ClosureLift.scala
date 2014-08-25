package doge.compiler.closures

import doge.compiler.types.TypeSystem.{Function, Type}
import doge.compiler.types._

/**
 * This is a phase of the compiler that ensure all closures
 * have a raw method which they can be lifted from.
 *
 * i.e. any built-in function is automatically encoded
 * in a local method which can be lifted.
 *
 * In the future, this should also ensure that any legitimate closures
 * are lifted.
 */
object ClosureLift {

  def liftClosures(module: ModuleTyped): ModuleTyped = {
    ModuleTyped(module.name, module.definitions.flatMap(d => lift(d, module.name)))
  }


  /** Extractor to look for a reference to a closure, including the number of arguments required
    * to pass into the reference until reaching the closure/function object.
    */
  object MethodReferenceType {
    import TypeSystem._
    def unapply(id: IdReferenceTyped): Option[Type] = {
      id.tpe match {
        case Function(_,_) => Some(id.tpe)
        case _ => None
      }
    }
  }

  object FunctionArgCount {
    import TypeSystem._
    def unapply(tpe: Type): Option[Int] = tpe match {
      case Function(arg, result) =>
        def countHelper(tpe: Type, count: Int): Int =
          tpe match {
            case Function(arg, result) => countHelper(result, count + 1)
            case _ => count
          }
        Some(countHelper(tpe, 0))
      case _ => None
    }
  }

  /**
   * Captures any function application which is partial.
   */
  object PartialApplication {
    def unapply(ast: TypedAst): Option[(ApExprTyped)] = ast match {
      case ap@ApExprTyped(MethodReferenceType(FunctionArgCount(argCount)), args, _, _) if args.size < argCount => Some(ap)
      case _ => None
    }
  }
  object IsBuiltIn {
    def unapply(ast: TypedAst): Boolean =
      ast match {
        case IdReferenceTyped(_, Location(BuiltIn), _) => true
        case ApExprTyped(IdReferenceTyped(_, Location(BuiltIn), _), _, _, _) => true
        case _ => false
      }
  }


  // NOTE: Type-preserving expansion of partial applications/lambdas into helper methods.
  def lift(l: LetExprTyped, moduleClassName: String): Seq[LetExprTyped] = {
    // TODO - super dirty evil impl, maybe have this be re-usable eventually)
    var additionalLets = Seq.empty[LetExprTyped]
    def liftImpl(expr: TypedAst): TypedAst =
       expr match {
         // TODO - We may need to figure out a mechanism to handle unbound args which is slightly better...
         case PartialApplication(ap @ IsBuiltIn()) =>
           // NOTE - Remember to recurse into methods
           val (allArgTypes, returnType) = deconstructArgs(ap.name.tpe)
           val liftedArgNames = allArgTypes.zipWithIndex.map(x => s"arg${x._2}" -> x._1)
           val lambdaMethodName = s"${l.name}$$lifted${additionalLets.size}"
           val lifted = LetExprTyped(lambdaMethodName, liftedArgNames.map(_._1),
             ApExprTyped(ap.name,
               for((name, tpe) <- liftedArgNames)
               yield IdReferenceTyped(name, TypeEnvironmentInfo(name, Argument, tpe)),
               returnType,
               ap.pos
             )
           , ap.name.tpe)
           additionalLets +:= lifted
           ApExprTyped(
             IdReferenceTyped(lambdaMethodName, TypeEnvironmentInfo(lambdaMethodName, StaticMethod(moduleClassName, lambdaMethodName, allArgTypes, returnType), ap.name.tpe)),
             ap.args.map(liftImpl),
             ap.tpe)
         case ApExprTyped(name, args, tpe, pos) => ApExprTyped(name, args.map(liftImpl), tpe, pos)
         case LetExprTyped(name, arg, defn, tpe, pos) => LetExprTyped(name, arg, liftImpl(defn), tpe, pos)
         case _ => expr
       }

    val result = liftImpl(l).asInstanceOf[LetExprTyped]
    additionalLets :+ result
  }


  // TODO - here we need to:
  // 1. Figure out all unbound argument tpyes and name them in the apply method.
  // 2. Find a way to encode all bound variables as AST nodes when we feed to existing classfile generation code.
  def deconstructArgs(tpe: Type): (Seq[Type], Type) = {
    def deconstructArgs(argList: Seq[Type], nextFunc: Type): Seq[Type] = nextFunc match {
      case Function(arg, next) => deconstructArgs(argList :+ arg, next)
      case result => argList :+ result
    }
    val all = deconstructArgs(Nil, tpe)
    (all.init, all.last)
  }

}
