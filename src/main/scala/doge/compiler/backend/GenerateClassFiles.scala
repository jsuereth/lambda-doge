package doge.compiler
package backend

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
      case Integer =>
        mws.mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;")
      case TypeSystem.Bool =>
        mws.mv.visitMethodInsn(INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;")
      case _ => // Ignore, we are already boxed
    }
    mws -> ()
  }
  // DUMB unboxing (lazy as possible)
  def unbox(tpe: Type) = State[MethodWriterState, Unit] { mws =>
    tpe match {
      case Integer =>
        mws.mv.visitTypeInsn(CHECKCAST, "java/lang/Integer")
        mws.mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Integer", "intValue", "()I")
      case TypeSystem.Bool =>
        mws.mv.visitTypeInsn(CHECKCAST, "java/lang/Boolean")
        mws.mv.visitMethodInsn(INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;")
      case _ => // Ignore, we are already boxed
    }
    mws -> ()
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
    state -> ()
  }

  /** using the IFEQ jump instruction, check the top of the stack and either jump or not. */
  def jumpIfEq(l: Label) = State[MethodWriterState, Unit] { state =>
    state.mv.visitJumpInsn(IFEQ, l)
    state -> ()
  }

  /** Goto the given label. */
  def goto(l: Label) = State[MethodWriterState, Unit] { state =>
    state.mv.visitJumpInsn(GOTO, l)
    state -> ()
  }

  /** Writes a label at the current bytecode location, for jump instructions. */
  def writeLabel(l: Label) = State[MethodWriterState, Unit] { state =>
    state.mv.visitLabel(l)
    state -> ()
  }


  /** Will invoke a method defined in the current classfile, with the given type. */
  def callStaticLocalMethod(name: String, args: Int, tpe: Type) = State[MethodWriterState, Unit] { state =>
    state.mv.visitMethodInsn(INVOKESTATIC, state.className, name, GenerateClassFiles.getFunctionSignature(tpe, args))
    state -> ()
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
      case i: IntLiteralTyped => loadConstant(new java.lang.Integer(i.value))
      case b: BoolLiteralTyped => loadConstant(new java.lang.Boolean(b.value))
      // TODO - how to handle id references?
      // TODO - move these into builtins...
      case ApExprTyped(i, Seq(id), _, _) if i.name == "IS" => placeOnStack(id)
      case ApExprTyped(id, args, _, _) if id.name == "PrintLn" => builtInFunctions.println(args)
      // This method is not built in.  For now, we assume any non-built-in method is defined
      // on the same classfile.
      // TODO - Handle non-local methods
      case ap @ ApExprTyped(id, args, tpe, _) if TypeSystem.Function.arity(id.tpe) == args.length => applyFunction(id, args)
      case ap @ ApExprTyped(id, args, tpe, _) => liftLambda(id, args, ap.pos)
      case i: IdReferenceTyped =>
        for {
          idx <- localVarIndex(i.name)
          _ <- idx match {
            case Some(idx) => loadLocalVariable(i.tpe, idx)
            case _ =>
              // Here we assume any non-local variable reference is a local
              // method call.
              // TODO - Allow non-local method calls/referencing members.
              // TODO - Lambda lift here
              callStaticLocalMethod(i.name, 0, i.tpe)
          }
        } yield ()

  })
  // TODO - Maybe create a new set of ASTs where we can lift lambdas prior to bytecode generation.
  import scala.util.parsing.input.Position
  def liftLambda(id: IdReferenceTyped, args: Seq[TypedAst], pos: Position): State[MethodWriterState, Unit] = {
    // TODO - implement a lifted lambda class and instantiate it.

    // Basic idea:
    // 1. Create a local static method which contains the actually function call, or inlined implementation.
    // 2. Create a class (or static inner class) which contains:
    //    - A method handle
    //    - An instance of all the *bound* arguments for the function
    //    - An apply method that takes the remaining (unbound) arguments.
    //    - A constructor which takes the method handle and all bound arguments
    // 3. Instantiate the anonymous class with the method handle and bound arguments here.
    sys.error(s"Lambda lifting not implemented at:\n${pos.longString}")
  }

  /** calls a function with a given reference and set of arguments. */
  def applyFunction(i: IdReferenceTyped, args: Seq[TypedAst]): State[MethodWriterState, Unit] = {
    // TODO _ Check to see if we need to lambda lift here.
    type S[A] = State[MethodWriterState, A]
    for {
      _ <- args.reverse.toList.traverse[S, Unit](placeOnStack)
      _ <- callStaticLocalMethod(i.name, args.size, i.tpe)
    } yield ()

  }


  type WrittenMethodState = State[MethodWriterState, Unit]


  /** TOOD - Unifiy with however Java types are exposed in the typesystem */
  def getStatic(className: String, field: String, tpeString: String) = State[MethodWriterState, Unit] { state =>
    state.mv.visitFieldInsn(GETSTATIC, className, field, tpeString)
    state -> ()
  }

  // TODO - Unify this with Java types as exposed in the typesystem later.
  def invokeVirtual(className: String, methodName: String, argCount: Int, tpe: Type) = State[MethodWriterState, Unit] { state =>
    state.mv.visitMethodInsn(INVOKEVIRTUAL, className, methodName, GenerateClassFiles.getFunctionSignature(tpe, argCount))
    state -> ()
  }


  object builtInFunctions {

    /** Loads the stdout variable. */
    def stdout = getStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
    /** Prints an expression. */
    def print(in: TypedAst): State[MethodWriterState, Unit] =
     for {
       out <- stdout
       _ <- placeOnStack(in)
       // TODO - convert to string if not already string.
       _ <- invokeVirtual("java/io/PrintStream", "print", 1, TypeSystem.Function(in.tpe, TypeSystem.Unit))
     } yield ()
    /** Writes the println method. */
    def println(args: Seq[TypedAst]): State[MethodWriterState, Unit] = {
      type S[A] = State[MethodWriterState, A]
      for {
        _ <- args.toList.traverse[S, Unit](print)
        _ <- emptyPrintln()
      } yield ()
    }

    /** Prints an empty line. */
    def emptyPrintln() =
      stdout flatMap { _ =>
        State[MethodWriterState,Unit] { state =>
          state.mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "()V")
          state -> ()
        }
      }

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
        state -> ()
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
    cw.visit(V1_6,
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
        "()V")
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


  private def writeClassFile(f: File, cw: ClassWriter): Unit = {
    val out = new FileOutputStream(f)
    try out.write(cw.toByteArray)
    finally out.close()
  }


  def isMainMethod(m: LetExprTyped): Boolean =
    (m.name == "main") && (m.tpe == Unit)

  import scala.util.parsing.input.Position
  def getFunctionSignature(tpe: Type, args: Int, pos: Option[Position ] = None): String = {
    import TypeSystem.Function
    val signature = new SignatureWriter()
    def visitFunctionSignature(signature: SignatureVisitor, f: Type, args: Int): Unit =
      f match {
        case f if args <= 0 =>
          visitSignatureInternal(signature.visitReturnType, f, pos)
        case Function(arg, next @ Function(_,_)) =>
          signature.visitParameterType()
          visitSignatureInternal(signature, arg, pos)
          visitFunctionSignature(signature, next, args - 1)
        case Function(arg, result) =>
          signature.visitParameterType()
          visitSignatureInternal(signature, arg, pos)
          visitFunctionSignature(signature, result, args - 1)
        // TODO _ Everything else is assumed to be an object
        case _ => sys.error(s"Reached end of function signature type, but not end of argument count required! type: $f")
      }
    visitFunctionSignature(signature, tpe, args)
    signature.toString
  }

  /** Represents the type of argument (or return type) for a method on the JVM */
  private[backend] def visitSignatureInternal(signature: SignatureVisitor, tpe: Type, pos: Option[Position] = None): Unit =
    BuiltInType.all.visitSignatureInternal.applyOrElse[(SignatureVisitor, Type), Unit]((signature, tpe), {
      case (sv, Unit) => signature.visitBaseType('V')
      // TODO - Don't hardcode the object string everywhere.
      // TODO - Eventually, we may want some kind of interface for objects...
      case (sv, f @ TypeSystem.Function(_, _)) => signature.visitClassType("java/lang/Object;")
      case _ => sys.error(s"Unsupported argument/return type: [$tpe]${pos.map(_.longString).map("\n"+).getOrElse("")}")
    })

}
