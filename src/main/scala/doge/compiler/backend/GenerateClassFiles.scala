package doge.compiler
package backend

import java.lang.invoke.LambdaMetafactory
import doge.compiler.classloader.ClassSymbolReader
import doge.compiler.symbols._
import doge.compiler.symbols.ScopeSymbolTable.{Argument => ArgumentSym, Function => FunctionSym}
import doge.compiler.std.BuiltInType
import org.objectweb.asm.Opcodes._
import org.objectweb.asm._
import doge.compiler.types._
import doge.compiler.types.TypeSystem.{TypeConstructor, Integer, Unit, Type}
import java.io._

import org.objectweb.asm.signature.{SignatureWriter, SignatureVisitor}
import scalaz._
import Scalaz._


case class MethodWriterState(className: String,
                             mv: MethodVisitor,
                             stackNameToIndex: Map[String, Int],
                             methodName: String,
                             lambdaClassIdx: Int = 0,
                             outputDirectory: File)

object MethodWriter {

  def classDirectory = State[MethodWriterState, File] { mws => mws -> mws.outputDirectory}
  // TODO - Better mechanism here
  def sourceFilename = State[MethodWriterState, String] { mws => mws -> s"${mws.className}.doge"}

  def localVarIndex(name: String) = State[MethodWriterState, Option[Int]] { mws =>
    // TODO - Possibly error out here?
    (mws, mws.stackNameToIndex.get(name))
  }


  /** Loads a local variable off the variable stack. */
  def loadLocalVariable(tpe: Type, idx: Int) = State[MethodWriterState, Unit] { mws =>
    tpe match {
      case TypeSystem.Bool => mws.mv.visitVarInsn(ILOAD, idx)
      case Integer => mws.mv.visitVarInsn(ILOAD, idx)
      case _ =>
        // We assume everything else is an object.
        mws.mv.visitVarInsn(ALOAD, idx)
    }
    (mws, ())
  }

  def rawInsn[A](f: MethodVisitor => A) = State[MethodWriterState, Unit] { mws =>
    mws -> f(mws.mv)
  }

  val dupe = rawInsn(_.visitInsn(DUP))
  val pop = rawInsn(_.visitInsn(POP))


  // DUMB boxing (lazy as possible)
  def box(tpe: Type) = State[MethodWriterState, Unit] { mws =>
    tpe match {
      case TypeSystem.Integer =>
        mws.mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false)
      case TypeSystem.Bool =>
        mws.mv.visitMethodInsn(INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;", false)
      case _ => // Ignore, we are already boxed
    }
    (mws, ())
  }
  // DUMB unboxing (lazy as possible)
  def unbox(tpe: Type) = State[MethodWriterState, Unit] { mws =>
    tpe match {
      case TypeSystem.Integer =>
        mws.mv.visitTypeInsn(CHECKCAST, "java/lang/Integer")
        mws.mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Integer", "intValue", "()I", false)
      case TypeSystem.Bool =>
        mws.mv.visitTypeInsn(CHECKCAST, "java/lang/Boolean")
        mws.mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Boolean", "booleanValue", "()Z", false)
      case _ => // Ignore, we are already boxed
    }
    (mws, ())
  }

  def className = State[MethodWriterState, String] { state =>
    state -> state.className
  }

  def nextLambdaClassName = State[MethodWriterState, String] { state =>
    val id = state.lambdaClassIdx + 1
    state.copy(lambdaClassIdx = id) -> s"${state.className}$$${state.methodName}$$fun$$${id}"
  }




  /**
   * Loads a local variable off the local variable stack, by name.
   *
   * Throws an exception if the name does not exist.
   * @param tpe
   * @param name
   * @return
   */
  def loadLocalVariable(tpe: Type, name: String): State[MethodWriterState, Unit] =
    for {
      idx <- localVarIndex(name)
      _ <- loadLocalVariable(tpe, idx.getOrElse(sys.error(s"Could not find local variable $name")))
    } yield ()


  // Injects the bytecode so the value returned by the given AST is pushed onto the stack.
  def loadConstant(i: AnyRef) = State[MethodWriterState, Unit] { state =>
    state.mv.visitLdcInsn(i)
    (state, ())
  }

  /** using the IFEQ jump instruction, check the top of the stack and either jump or not. */
  def jumpIfEq(l: Label) = State[MethodWriterState, Unit] { state =>
    state.mv.visitJumpInsn(IFEQ, l)
    (state, ())
  }

  /** Goto the given label. */
  def goto(l: Label) = State[MethodWriterState, Unit] { state =>
    state.mv.visitJumpInsn(GOTO, l)
    (state, ())
  }

  /** Writes a label at the current bytecode location, for jump instructions. */
  def writeLabel(l: Label) = State[MethodWriterState, Unit] { state =>
    state.mv.visitLabel(l)
    (state, ())
  }


  /** Wrotes an invokestatic call, all arguments must already be on the stack. */
  def callStaticMethod(m: FunctionSym) = State[MethodWriterState, Unit] { state =>
    state.mv.visitMethodInsn(INVOKESTATIC, m.ownerClass, m.name, GenerateClassFiles.getMethodSignature(m.argTpes, m.returnTpe), false)
    (state, ())
  }

  def callStaticField(f: JavaFieldSymbol) = State[MethodWriterState, Unit] { state =>
    assert(f.isStatic, s"Field is not static: $f")
    state.mv.visitFieldInsn(GETSTATIC, f.owner.jvmName, f.name, f.jvmDesc)
    (state, ())
  }

  /** Creates a new java instance on the stack of a class, without calling any constructor. */
  def createNewInstance(s: JavaConstructorSymbol) = State[MethodWriterState, Unit] { state =>
    state.mv.visitTypeInsn(Opcodes.NEW, s.owner.jvmName)
    (state, ())
  }

  /** Writes the java constructor call. */
  def callJavaConstructor(s: JavaConstructorSymbol) = State[MethodWriterState, Unit] { state =>
    state.mv.visitMethodInsn(Opcodes.INVOKESPECIAL, s.owner.jvmName, ClassSymbolReader.CONSTRUCTOR_NAME, s.jvmDesc, false)
    (state, ())
  }

  /** Writes a jave method call.  Either INVOKESTATIC, INVOKEINTERFACE or INVOKEVIRTUAL. */
  def callJavaMethod(s: JavaMethodSymbol) = State[MethodWriterState, Unit] { state =>
    val op =
      if(s.isStatic) Opcodes.INVOKESTATIC
      else {
        // TODO - invoke interface, or invoke special
        if(s.owner.isInterface) Opcodes.INVOKEINTERFACE
        else Opcodes.INVOKEVIRTUAL
      }
    state.mv.visitMethodInsn(op, s.owner.jvmName, s.name, s.jvmDesc, s.owner.isInterface)
    (state, ())
  }

  /** The is an extractor to determine the number of arguments to call the raw method.
    *
    */
  object ClosureReference {
    import TypeSystem._
    /** Given an id (and its symbol) determine how many physical arguments the JVM type takes. */
    def unapply(id: IdReferenceTyped): Option[Int] = {
      def unapplySym(sym: DogeSymbol): Option[Int] =
      // TODO - Make better symbol table readers here, and handle java symbols.
      sym.original match {
        case FunctionSym(_, argTpes, Function(_,_), _) => Some(argTpes.size)
        case x: FunctionSym => None
        case x: FunctionParameterSymbol =>
          // Note - we check the underlying type to see if it's paaramter, but we need to check the *inferred* (final)
          sym.tpe match {
            case Function(_,_) => Some(0)
            case _ => None
          }
        case x if x.isBuiltIn => None
        // Here we need to handle java types
        case x: JavaConstructorSymbol => Some(x.arity)
        case x: JavaFieldSymbol => if(x.isStatic) None else Some(1)
        case x: JavaMethodSymbol => Some(x.arity)
      }
      unapplySym(id.sym)
    }
  }
  // TODO - Somehow this isn't catching arguments that are closures...
  object ClosureCall {
    def unapply(ast: TypedAst): Option[(ApExprTyped, Int)] = ast match {
      case ap @ ApExprTyped(ClosureReference(argCount), args, _, _) =>
        if(args.size > argCount) Some(ap, argCount)
        else None
      case _ => None
    }
  }

  /** Places the result of an Ast expression onto the JVM stack.
    *
    * Note: This will either:
    *    * load a constant
    *    * Call a function
    *    * Lookup a local variable
    *    * run inlined function code (e.g. if method).
    * @param ast
    * @return
    */
  def placeOnStack(ast: TypedAst): State[MethodWriterState, Unit] = BuiltInType.all.backend.applyOrElse[TypedAst, State[MethodWriterState, Unit]](ast, {
      //  Literals are easiest to encode.
      case i: IntLiteralTyped => loadConstant(new java.lang.Integer(i.value))
      case b: BoolLiteralTyped => loadConstant(new java.lang.Boolean(b.value))
      case s: StringLiteralTyped => loadConstant(s.value)
      // TODO - how to handle id references?
      // TODO - move these into builtins...
      case ApExprTyped(i, Seq(id), _, _) if i.name == "IS" => placeOnStack(id)


      // Now we handle simple "reference" type lookups.

      // This is references an expression with no arguments, we just call the method to evaluate it.
        // TODO - this should handle all static methods, not just doge defined ones
      case IdReferenceTyped(s: FunctionSym, _) => callStaticMethod(s)
      // We should only hit this if we refernce a constructor of no arguments....
      case IdReferenceTyped(s: JavaConstructorSymbol, _) =>
        for {
          _ <- createNewInstance(s)
          _ <- dupe
          _ <- callJavaConstructor(s)
        } yield ()

      case IdReferenceTyped(f: JavaFieldSymbol, _) => callStaticField(f)

      // Here we look up method arguments
      case IdReferenceTyped(sym, pos) if sym.original.isInstanceOf[ArgumentSym] =>
        for {
          idx <- localVarIndex(sym.name)
          _ <- idx match {
            case Some(i) => loadLocalVariable(ast.tpe, i)
            case None => sys.error(s"Unable to find argument [${sym.name}] when generating method bytecode at:\n$pos.longString}")
          }
        } yield ()


      // Now we have special treatment for Closure calls.  i.e.
      // we need to use java.util.function.Function.apply rather than just straight JVM dispatch.
      case ClosureCall(ap, argsToClosure) => callClosure(ap, argsToClosure)


      // Here we have a straight up method call with all argumnets known.
      case ApExprTyped(IdReferenceTyped(s @ FunctionSym(_, argTypes, _, _), _), args, tpe, _) if argTypes.length == args.length =>
        type MWS[A] = State[MethodWriterState, A]
        for {
          _ <- args.toList.traverse[MWS, Unit](placeOnStack)
          _ <- callStaticMethod(s)
        } yield ()

      case ApExprTyped(IdReferenceTyped(s: JavaConstructorSymbol, _), args, tpe, _) if s.arity == args.length =>
        type MWS[A] = State[MethodWriterState, A]
        for {
          _ <- createNewInstance(s)
          _ <- dupe
          _ <- args.toList.traverse[MWS, Unit](placeOnStack)
          _ <- callJavaConstructor(s)
        } yield ()

      case ApExprTyped(IdReferenceTyped(s: JavaMethodSymbol, _), args, tpe, _) if s.arity == args.length =>
        type MWS[A] = State[MethodWriterState, A]
        for {
          _ <- args.toList.traverse[MWS,Unit](placeOnStack)
          _ <- callJavaMethod(s)
        } yield ()

      // Partial Application.
      // Now we need to lift closures.  All built-in expressions should already have been handled.
      // Note: we may be lifting built-in expressions into closures...
      case ap @ ApExprTyped(id, args, tpe, _) => liftClosure(id, args, tpe, ap.pos)

      case _ => sys.error(s"Unable to handle ast: $ast")
  })


  /** Makes a closure method call, a.k.a against a closure instance
    * TODO - Maybe we should automatically convert closure application trees into TWO method calls,
    * one which is a regular dispatch and one which is Function application before we get to this stage,
    * rather than using this complex logic here.
    */
  def callClosure(ap: ApExprTyped, argsToClosure: Int): State[MethodWriterState, Unit] = {
    // Alg  - First we call all arguments up until we have a closure on the stack
    //        Second, we invokedynamic the apply method with remaining unbound arguments.
    def closureExpr = {
      val realArgsToClosure = ap.args.take(argsToClosure)
      // TOOD - Don't hardcode a bad type here.
      ApExprTyped(ap.name, realArgsToClosure, ap.name.tpe)
    }
    val argsForClosure = ap.args.drop(argsToClosure )
    type MWS[A]=State[MethodWriterState, A]
    // Helper method to pass each closure argument into the curried function.
    def partiallyApply(arg: TypedAst): MWS[Unit] = {
      for {
        _ <- placeOnStack(arg)
        _ <- box(arg.tpe)
        _ <- rawInsn(_.visitMethodInsn(INVOKEINTERFACE, "java/util/function/Function", "apply", "(Ljava/lang/Object;)Ljava/lang/Object;", true))
      } yield ()
    }
    def placeClosureOnStack =
      if(argsToClosure == 0) placeOnStack(ap.name)
      else placeOnStack(closureExpr)

    for {
      _ <- placeClosureOnStack
      _ <- argsForClosure.toList.traverse[MWS, Unit](partiallyApply)
    // TODO - unbox to result type or the application...
     _ <- unbox(ap.tpe)
    } yield  ()
  }


  // TODO - Maybe create a new set of ASTs where we can lift lambdas prior to bytecode generation.
  import scala.util.parsing.input.Position

  // Note this is hardcoded from laziness.
  private val metaFactoryHandle: Handle =
    new Handle(
      Opcodes.H_INVOKESTATIC,
      "java/lang/invoke/LambdaMetafactory",
      "metafactory",
      "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;")


  /**
   * THis will lift a java.util.function.Function class from a partially-applied function call.
   *
   * Requirements:
   *   1. The referenced method must have ALL arguments satisified but one. i.e. id.args == args.size-1
   *   2. The referenced method must not be built-in or an argument.
   *
   * Here we use invokeDynamic and Java's metaFactoryHandle to lift a closure (with one captured arg)
   * for the given method.   This will place a `java.util.function.Function<?,?>` object on the stack.
   */
  def liftClosure(id: IdReferenceTyped, args: Seq[TypedAst], resultType: Type, pos: Position): State[MethodWriterState, Unit] = {
    type MWS[A] = State[MethodWriterState, A]
    for {
      _ <- args.toList.traverse[MWS, Unit](placeOnStack)
      cn <- className
      _ <- rawInsn { mv =>
        import org.objectweb.asm.commons.GeneratorAdapter
        // A method handle on the function we're lifting into a closure.
        val delegateMethodHandle: Handle = methodHandleFromSymbol(id.sym, pos)
        // TODO - Will we always be returning an object?  Perhaps
        val inputType: org.objectweb.asm.Type = org.objectweb.asm.Type.getType("(Ljava/lang/Object;)Ljava/lang/Object;")
        // TODO - This needs to be the type of the resulting closure.
        val outputType: org.objectweb.asm.Type = org.objectweb.asm.Type.getType(GenerateClassFiles.getFunctionSignature(resultType, 1))
        // Our bootstrap signature takes in one argument and returns a function, so...
        val bootStrapSignature =
          GenerateClassFiles.getMethodSignature(args.map(_.tpe), TypeSystem.Function(TypeSystem.newVariable, TypeSystem.newVariable))
          //"(Ljava/util/function/Function;)Ljava/util/function/Function;"
        // TODO - get actual method type
        mv.visitInvokeDynamicInsn(
          "apply",
          bootStrapSignature,
          metaFactoryHandle,
          inputType,
          delegateMethodHandle,
          outputType
        );
      }
    } yield ()
  }

  /** Generates a method handle from the symbol we have.
    */
  private def methodHandleFromSymbol(sym: DogeSymbol, pos: Position): Handle =
   sym match {
     case FunctionSym(mthd, args, result, cls) =>
       new Handle(Opcodes.H_INVOKESTATIC, cls, mthd, GenerateClassFiles.getFunctionSignature(TypeSystem.FunctionN(result, args:_*), args.size))
     case _ => sys.error(s"We don't handle methods of type: ${sym}, at\n ${pos.longString}")
   }

  /** calls a function with a given reference and set of arguments. */
  def applyFunction(i: IdReferenceTyped, args: Seq[TypedAst]): State[MethodWriterState, Unit] = {
    // TODO _ Check to see if we need to lambda lift here.
    type S[A] = State[MethodWriterState, A]
    for {
      _ <- args.reverse.toList.traverse[S, Unit](placeOnStack)
      _ <- i.sym match {
        case s: FunctionSym => callStaticMethod(s)
        case _ => sys.error(s"Unable to determine how to invoke method $i")
      }
    } yield ()

  }


  type WrittenMethodState = State[MethodWriterState, Unit]


  // TOOD - Unify with however Java types are exposed in the typesystem
  def getStatic(className: String, field: String, tpeString: String) = State[MethodWriterState, Unit] { state =>
    state.mv.visitFieldInsn(GETSTATIC, className, field, tpeString)
    (state, ())
  }

  // TODO - Unify this with Java types as exposed in the typesystem later.
  def invokeVirtual(className: String, methodName: String, argCount: Int, tpe: Type, isInterface: Boolean) = State[MethodWriterState, Unit] { state =>
    state.mv.visitMethodInsn(INVOKEVIRTUAL, className, methodName, GenerateClassFiles.getFunctionSignature(tpe, argCount), isInterface)
    (state, ())
  }

  /** writes a method definition using the given state, returning the last expression result. */
  def writeMethod(defn: TypedAst): State[MethodWriterState, Unit] = {
    placeOnStack(defn).flatMap { _ =>
      State[MethodWriterState, Unit] { state =>
        // TODO - This should return the result of the expression type, not just integer always.
        defn.tpe match {
          case TypeSystem.Integer => state.mv.visitInsn(IRETURN)
          case TypeSystem.Bool => state.mv.visitInsn(IRETURN)
          case TypeSystem.Unit => state.mv.visitInsn(RETURN)
          // Everything else is an object
          case _ => state.mv.visitInsn(ARETURN)
        }
        // We need this here, even if it's wrong, as ASM will calculate it appropriately.
        state.mv.visitMaxs(1, 1);
        state.mv.visitEnd()
        (state, ())
      }
    }
  }

}

// TODO - Rename to classfileWriter, possibly with its own state.
object GenerateClassFiles {
  import Opcodes._

  def makeClassfile(module: ModuleTyped, dir: File): File = {
    val file = new java.io.File(dir, module.name + ".class")
    val cw = new ClassWriter((ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS))
    cw.visit(V1_7,
      ACC_PUBLIC + ACC_SUPER,
      module.name,
      null,
      "java/lang/Object",
      null)
    cw.visitSource(s"${module.name}.doge", null)
    visitEmptyConstructor(cw)
    // Now we write the methods.
    module.definitions.foreach(m => writeMethod(m, module.name, cw, dir))
    cw.visitEnd()
    writeClassFile(file, cw)
    file
  }

  // We ain't never gon be real class, but de javas,she needs a comstruktar.
  private def visitEmptyConstructor(cw: ClassWriter): Unit = {
      val mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)
      mv.visitVarInsn(ALOAD, 0)
      mv.visitMethodInsn(INVOKESPECIAL,
        "java/lang/Object",
        "<init>",
        "()V", false)
      mv.visitInsn(RETURN)
      mv.visitMaxs(1, 1)
      mv.visitEnd()
  }

  private def mainMethod(defn: TypedAst, cw: ClassWriter, className: String, target: File): Unit = {
    // Handle this specially.. For now, just always hello world.
    val mv = cw.visitMethod(ACC_PUBLIC + ACC_STATIC,
      "main",
      "([Ljava/lang/String;)V",
      null,
      null);
    val state = MethodWriterState(className, mv, Map.empty, "main", 0, target)
    MethodWriter.writeMethod(defn)(state)
  }

  private def writeMethod(ast: LetExprTyped, name: String, cw: ClassWriter, target: File): Unit = {
    if(isMainMethod(ast)) mainMethod(ast.definition, cw, name, target)
    else {
      // TODO - we need to handle the possibility of returning functions here.
      // At a minimum, we need to identify argument types vs. return types.
      val signature = getFunctionSignature(ast.tpe, ast.argNames.size, Some(ast.pos))
      // TODO - generic signature...
      val mv = cw.visitMethod(ACC_PUBLIC + ACC_STATIC,
         ast.name,
         signature,
         null,
         null)
      val state = MethodWriterState(name, mv, ast.argNames.zipWithIndex.toMap, ast.name, 0, target)
      MethodWriter.writeMethod(ast.definition)(state)
    }
  }


  private[backend] def writeClassFile(f: File, cw: ClassWriter): Unit = {
    val out = new FileOutputStream(f)
    try out.write(cw.toByteArray)
    finally out.close()
  }


  def isMainMethod(m: LetExprTyped): Boolean =
    (m.name == "main") && (m.tpe == Unit)

  import scala.util.parsing.input.Position
  def getFunctionSignature(tpe: Type, args: Int, pos: Option[Position] = None): String = {
    import TypeSystem.Function
    val signature = new SignatureWriter()
    def visitFunctionSignature(signature: SignatureVisitor, f: Type, a: Int): Unit =
      f match {
        case f if a <= 0 =>
          visitSignatureInternal(signature.visitReturnType, f, pos)
        case Function(arg, next @ Function(_,_)) =>
          signature.visitParameterType()
          visitSignatureInternal(signature, arg, pos)
          visitFunctionSignature(signature, next, a - 1)
        case Function(arg, result) =>
          signature.visitParameterType()
          visitSignatureInternal(signature, arg, pos)
          visitFunctionSignature(signature, result, a - 1)
        // TODO _ Everything else is assumed to be an object
        case _ => sys.error(s"Reached end of function signature type, but not end of argument count required! type: $tpe, expected args: $args")
      }
    visitFunctionSignature(signature, tpe, args)
    signature.toString
  }

  def getFieldSignature(tpe: Type): String = {
    val signature = new SignatureWriter()
    visitSignatureInternal(signature, tpe)
    signature.toString
  }

  def getMethodSignature(args: Seq[Type], result: Type): String = {
    val signature = new SignatureWriter()
    args.foreach(arg => visitSignatureInternal(signature.visitParameterType, arg))
    visitSignatureInternal(signature.visitReturnType(), result)
    signature.toString
  }

  /** Represents the type of argument (or return type) for a method on the JVM */
  private[backend] def visitSignatureInternal(signature: SignatureVisitor, tpe: Type, pos: Option[Position] = None): Unit =
    BuiltInType.all.visitSignatureInternal.applyOrElse[(SignatureVisitor, Type), Unit]((signature, tpe), {
      case (sv, Unit) => signature.visitBaseType('V')
      // TODO - Don't hardcode the object string everywhere.
      // TODO - Eventually, we may want to show the generics.
      case (sv, f @ TypeSystem.Function(arg, result)) =>

        //signature.visitClassType("java/lang/Object;")
        signature.visitClassType("java/util/function/Function;")
        //visitSignatureInternal(signature.visitTypeArgument('='), arg, pos)
        //visitSignatureInternal(signature.visitTypeArgument('='), result, pos)
      // TODO - is it ok to handle variable types as just objects?
      case (sv, _: TypeSystem.TypeVariable) => sv.visitClassType("java/lang/Object;")
      // TODO - For all other types, we either try to convert them into java types *or*  bail
      case (sv, TypeSystem.TypeConstructor(name, _)) if name contains "." =>
        // TODO - We may want to visit the generic signature here at some point.
        sv.visitClassType(name.replaceAllLiterally(".", "/"))
        sv.visitEnd()
      case _ => sys.error(s"Unsupported argument/return type: [$tpe]${pos.map(_.longString).map("\n"+).getOrElse("")}")
    })

}
