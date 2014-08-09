package doge.compiler.backend

import org.objectweb.asm._
import doge.compiler.types._
import doge.compiler.types.TypeSystem.{
  Integer, Unit, Type
}
import java.io._

import org.objectweb.asm.signature.{SignatureWriter, SignatureVisitor}


object GenerateClassFiles {
  import Opcodes._

  def makeClassfile(methods: Seq[LetExprTyped], dir: File, name: String): File = {
    val file = new java.io.File(dir, name + ".class")
    val cw = new ClassWriter((ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS))
    cw.visit(V1_6,
      ACC_PUBLIC + ACC_SUPER,
      name,
      null,
      "java/lang/Object",
      null)
    cw.visitSource(s"$name.doge", null)
    visitEmptyConstructor(cw)
    // Now we write the methods.
    methods.foreach(m => writeMethod(m, name, cw))
    cw.visitEnd()
    writeClassFile(file, cw)
    file
  }

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

  private def mainMethod(defn: TypedAst, cw: ClassWriter, className: String): Unit = {
    // Handle this specially.. For now, just always hello world.
    val mv = cw.visitMethod(ACC_PUBLIC + ACC_STATIC,
      "main",
      "([Ljava/lang/String;)V",
      null,
      null);
    writeStackInstructions(defn, new EvilMethodWriterState(mv, className))
    mv.visitInsn(RETURN);
    mv.visitMaxs(2, 1);
    mv.visitEnd();
  }

  private def writeMethod(ast: LetExprTyped, name: String, cw: ClassWriter): Unit = {
    if(isMainMethod(ast)) mainMethod(ast.definition, cw, name)
    else {
      // TODO - generic write method impl...
      val signature = getFunctionSignature(ast.tpe)
      // TODO - generic signature...
      System.err.println(s"Creating method: ${ast.name} $signature")
      val mv = cw.visitMethod(ACC_PUBLIC + ACC_STATIC,
         ast.name,
         signature,
         null,
         null)
      writeMethodinstructions(ast.argNames, ast.definition, new EvilMethodWriterState(mv, name))
      mv.visitEnd()
    }
  }

  // TODO - use scalaz state
  class EvilMethodWriterState(val mv: MethodVisitor, val className: String) {
    private val lookUps = new java.util.HashMap[String, Int]
    def setConstant(name: String, idx: Int): Unit = {
      lookUps.put(name, idx)
    }
    def getConstantLocation(name: String): Option[Int] =
      Option(lookUps.get(name))
  }

  private def writeMethodinstructions(argNames: Seq[String], defn: TypedAst, state: EvilMethodWriterState): Unit = {
    // First update evil state with method argument locations.
    for((arg, idx) <- argNames.zipWithIndex) {
      state.setConstant(arg, idx)
    }
    writeStackInstructions(defn, state)
    //mv.visitLdcInsn(1)
    state.mv.visitInsn(IRETURN)
    // We need this here, even if it's wrong, as ASM will calculate it appropriately.
    state.mv.visitMaxs(1, 1);
  }

  private def writeStackInstructions(defn: TypedAst, state: EvilMethodWriterState): Unit =
    defn match {
      // Literals are easiest for now...
      case i: LiteralTyped => placeOnStack(i, state)
      // Plus is hardcoded
      case ApExprTyped(id, Seq(left, right), _) if id.name == "Plus" =>
        placeOnStack(left, state)
        placeOnStack(right, state)
        state.mv.visitInsn(IADD)
      // Never call id, just write the stack instructions for the nested expression.
      case ApExprTyped(i, Seq(id), _) if i.name == "IS" => writeStackInstructions(id, state)
      // Println is hardcoded
      case ApExprTyped(id, args, _) if id.name == "PrintLn" =>
        // SUPER LAZY IMPL TIME
        import state.mv
        def getSystemOut() = mv.visitFieldInsn(GETSTATIC,
          "java/lang/System",
          "out",
          "Ljava/io/PrintStream;");
        // TODO - Coerce args into strings
        for(arg <- args) {
          getSystemOut
          placeOnStack(arg, state)
          // TODO - look up the correct type, for now we only support Int.
          mv.visitMethodInsn(INVOKEVIRTUAL,
            "java/io/PrintStream",
            "print",
            "(I)V"
          )
        }
        // TODO - string concat
        // TODO - call printlns
        // Finally we just println
        getSystemOut
        mv.visitMethodInsn(INVOKEVIRTUAL,
          "java/io/PrintStream",
          "println",
          "()V");

      // Application is not built in, we should create a method call here.
      case ApExprTyped(id, args, _) =>
        // Now we should call the method
        args.foreach(arg => placeOnStack(arg, state))
        state.mv.visitMethodInsn(INVOKESTATIC,
          state.className,
          id.name,
          getFunctionSignature(id.tpe))
    }

  private def placeOnStack(ast: TypedAst, state: EvilMethodWriterState): Unit =
    ast match {
      case i: IntLiteralTyped => state.mv.visitLdcInsn(i.value)
      // TODO - how to handle id references?
      case i: IdReferenceTyped =>
        state.getConstantLocation(i.name) match {
          case Some(idx) => typeToLoadInstruction(state.mv, i.tpe, idx)
          case _ => // Call the method?
            // Here we assume this is a previous let expression.
            state.mv.visitMethodInsn(INVOKESTATIC,
              state.className,
              i.name,
              getFunctionSignature(i.tpe))
        }
      case other => writeStackInstructions(other, state)
    }

  def typeToLoadInstruction(mv: MethodVisitor, tpe: Type, idx: Int): Unit = tpe match {
    case Integer => mv.visitVarInsn(ILOAD, idx)
    case _ =>
      // We assume everything else is an object.
      mv.visitVarInsn(ALOAD, idx)
  }

  // TODO - Support functions w/ generic parameters.
  def getFunctionSignature(tpe: Type): String = {
    import TypeSystem.Function
    val signature = new SignatureWriter()
    def visitFunctionSignature(signature: SignatureWriter, f: Type): Unit =
      f match {
        case Function(arg, next @ Function(_,_)) =>
          signature.visitParameterType()
          visitInternal(signature, arg)
          visitFunctionSignature(signature, next)
        case Function(arg, result) =>
          signature.visitParameterType()
          visitInternal(signature, arg)
          visitFunctionSignature(signature, result)
        case Integer => signature.visitReturnType().visitBaseType('I')
        case Unit => signature.visitReturnType().visitBaseType('V')
      }
    def visitInternal(signature: SignatureWriter, tpe: Type): Unit = tpe match {
      case Integer => signature.visitBaseType('I')
      case Unit => signature.visitBaseType('V')
      // We don't really support passing functions yet.
      case f @ TypeSystem.Function(_, _) => signature.visitBaseType('A')
      case _ => sys.error(s"Unsupported type: $tpe")
    }
    visitFunctionSignature(signature, tpe)
    signature.toString
  }


  private def writeClassFile(f: File, cw: ClassWriter): Unit = {
    val out = new FileOutputStream(f)
    try out.write(cw.toByteArray)
    finally out.close()
  }


  def isMainMethod(m: LetExprTyped): Boolean =
    (m.name == "main") && (m.tpe == Unit)


  def main(args: Array[String]): Unit = {
    import TypeSystem._
    val test = FunctionN(Integer, Integer, Integer)
    println(s"$test signature = ${getFunctionSignature(test)}")
  }
}
