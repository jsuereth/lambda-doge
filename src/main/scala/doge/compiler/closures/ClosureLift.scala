package doge.compiler.closures

import doge.compiler.symbols.ScopeSymbolTable.{Argument => ArgumentSym, Function => FunctionSym}
import doge.compiler.types.TypeSystem.{Function, Type}
import doge.compiler.types._

/**
 * This is a phase of the compiler that ensure all closures
 * have a raw method which they can be lifted from.
 *
 * i.e. any built-in function is automatically encoded
 * in a local method which can be lifted.
 *
 * This handles not just partially applied methods, it (TBD) handles closure syntax.
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

  object PartialApplicationMoreThanOneArg {
    def unapply(ast: TypedAst): Option[(ApExprTyped)] = ast match {
      case ap@ApExprTyped(MethodReferenceType(FunctionArgCount(argCount)), args, _, _) if args.size + 1 < argCount => Some(ap)
      case _ => None
    }
  }
  object IsBuiltIn {
    def unapply(ast: TypedAst): Boolean =
      ast match {
        case IdReferenceTyped(sym, _) => sym.isBuiltIn
        case ApExprTyped(IdReferenceTyped(sym, _), _, _, _) => sym.isBuiltIn
        case _ => false
      }
  }


  // NOTE: Type-preserving expansion of partial applications/lambdas into helper methods.
  def lift(l: LetExprTyped, moduleClassName: String): Seq[LetExprTyped] = {
    // TODO - super dirty evil impl, maybe have this be re-usable eventually)
    var additionalLets = Seq.empty[LetExprTyped]
    val newMethodCount = new java.util.concurrent.atomic.AtomicLong(0L)
    def makeMethodName(n: String): String =
      s"${n}$$lambda$$${newMethodCount.getAndIncrement}"
    val newCurriedCount = new java.util.concurrent.atomic.AtomicLong(0L)
    def makeCurriedMethodName(n: String): String =
      s"${n}$$curied$$${newCurriedCount.getAndIncrement}"

    // NOTE - There's an error here where argument types are erased somehow...
    def liftImpl(expr: TypedAst): TypedAst =
       expr match {
         // ALL lambda expressions are lifted into helper methods
         case le: LambdaExprTyped =>
           val (ap, let) = createClosure(le, moduleClassName, makeMethodName(l.name))
           // Delegate any partial funcion work to ouselves.
           additionalLets = liftImpl(let).asInstanceOf[LetExprTyped] +: additionalLets
           ap
         // If we have a partial application of a built-in method, we need to construct
         // a raw method which we can lift.
         case PartialApplication(ap @ IsBuiltIn()) =>
           // NOTE - Remember to recurse into methods
           val (allArgTypes, returnType) = deconstructArgs(ap.name.tpe)
           val liftedArgNames = allArgTypes.zipWithIndex.map(x => s"arg${x._2}" -> x._1)
           val lambdaMethodName = makeMethodName(l.name)
           // The underlying lambda we're lifting.
           val liftedZero = {
             val args: Seq[TypedAst] =
               for((name, tpe) <- liftedArgNames)
               yield IdReferenceTyped(ArgumentSym(name, tpe), ap.pos)
             LetExprTyped(lambdaMethodName, liftedArgNames.map(_._1),
               ap.name.tpe,
               ApExprTyped(ap.name, args, returnType, ap.pos),
               ap.pos)
           }
           additionalLets = liftedZero +: additionalLets
           // Now we lift this non-built-in partial application into helper lambdas recursively.
           val lambdaReferenceSym = FunctionSym(lambdaMethodName, allArgTypes, returnType, moduleClassName)
           liftImpl(ApExprTyped(
             IdReferenceTyped(lambdaReferenceSym, ap.name.pos),
             ap.args.map(liftImpl),
             ap.tpe))
         // We only need to lift if the partial application is more than one extra method.
         case PartialApplicationMoreThanOneArg(ap) =>
           // TODO - Peel of one argyent, and lift.
           val newArgLength = ap.args.length + 1
           val (allArgTypes, resultType) = deconstructArgs(ap.name.tpe, newArgLength)
           val newMethodName = makeCurriedMethodName(l.name)
           val newArgList = allArgTypes.zipWithIndex.map(x => s"arg${x._2}" -> x._1)

           val liftedDelegate = {
             val args: Seq[TypedAst] =
               for((name, tpe) <- newArgList)
               yield {
                 val argSym = ArgumentSym(name, tpe)
                 IdReferenceTyped(argSym)
               }
             LetExprTyped(newMethodName, newArgList.map(_._1),
               ap.name.tpe,
               ApExprTyped(ap.name,
                 args,
                 resultType,  // TODO - fix this?
                 ap.pos
               ))
           }
           additionalLets = liftImpl(liftedDelegate).asInstanceOf[LetExprTyped] +: additionalLets
           // TODO - figure out the type of the new function...
           val newMethodSym = FunctionSym(newMethodName, allArgTypes, resultType, moduleClassName)
           ApExprTyped(
             IdReferenceTyped(newMethodSym),
             ap.args.map(liftImpl),
             ap.tpe)
         case ApExprTyped(name, args, tpe, pos) => ApExprTyped(name, args.map(liftImpl), tpe, pos)
         case LetExprTyped(name, arg, tpe, defn, pos) => LetExprTyped(name, arg, tpe, liftImpl(defn), pos)
         case _ => expr
       }

    val result = liftImpl(l).asInstanceOf[LetExprTyped]
    result +: additionalLets
  }


  /** Constructs a new method for a lambda expression *AND* an application expression which
    * calls the underlying method.
    */
  def createClosure(l: LambdaExprTyped, className: String, methodName: String): (ApExprTyped, LetExprTyped) = {
    // TODO - Implement this!
    val closedArgs = closedRefs(l.definition).filterNot(r => l.argNames.contains(r.name))
    val (allArgTypes, returnType) = deconstructArgs(l.tpe, l.argNames.size)
    val unclosedArgs =
      for {
        (name, tpe) <- l.argNames.zip(allArgTypes)
      } yield IdReferenceTyped(ArgumentSym(name, tpe), l.pos)

    val allArgNames =
      (closedArgs ++ unclosedArgs).map(_.name)
    val implMethod = LetExprTyped(
       name = methodName,
       allArgNames,
       l.tpe,
       l.definition,
       l.pos
    )
    // TODO - The type for this method needs to be very different.  i.e. We need to construct
    // arg types from all closed variables and such.  For now, we'll cheat and see what happens.
    val implMethodRef = IdReferenceTyped(FunctionSym(methodName, allArgTypes, returnType, className))
    val callImplMethodExpr =
      ApExprTyped(
         implMethodRef,
         closedArgs,
         l.tpe
      )
    (callImplMethodExpr, implMethod)
  }

  // TODO - this doesn't really handle scoping well...
  def closedRefs(l: TypedAst): Seq[IdReferenceTyped] = l match {
    case i: IdReferenceTyped if i.sym.isInstanceOf[ArgumentSym] => Seq(i)
    case ApExprTyped(id, args, _, _) => args.flatMap(closedRefs) ++ closedRefs(id)
    // TODO - handle nested lambda expressions *OR* ensure we always work from deepest to outer scope.
    // We specifically ignore let expressions
    case _ => Nil
  }



  // Here we need to:
  // 1. Figure out all unbound argument tpyes and name them in the apply method.
  // 2. Find a way to encode all bound variables as AST nodes when we feed to existing classfile generation code.
  def deconstructArgs(tpe: Type): (Seq[Type], Type) = Function.deconstructArgs(tpe)()

  // Similar to above, but with a limited amount of deconstruction.
  def deconstructArgs(tpe: Type, args: Int): (Seq[Type], Type) = Function.deconstructArgs(tpe)(args)
}
