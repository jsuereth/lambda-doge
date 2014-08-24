package doge.compiler.backend

import doge.compiler.types._
import doge.compiler.types.TypeSystem._
import java.io.File

import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._


object LambdaWriter {

  val APPLY_METHOD_NAME = "apply"

  // TODO - Maybe hide this ina  state monad too...
  def writeLambdaClass(func: IdReferenceTyped, boundArgs: Seq[Type], name: String, source: String, classDir: File): File = {
    val argNames: Map[String, Type] =
      (for {
          (tpe, idx) <- boundArgs.zipWithIndex
       } yield s"arg$idx" -> tpe)(collection.breakOut)
    val classFile = new java.io.File(classDir, name + ".class")
    val cw = new ClassWriter((ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS))
    cw.visit(V1_7,
      ACC_PUBLIC + ACC_SUPER,
      name,
      null,
      "java/lang/Object",
      null)
    cw.visitSource(s"${source}", null)
    // Now we write the fields
    visitLambdaFields(cw, argNames)
    // Now we need to write the constructor
    visitLambdaConstructor(cw, name, argNames)
    // Now we write the methods.
    visitLambdaApplyMethod(cw, name, func, argNames, classDir)

    cw.visitEnd()
    GenerateClassFiles.writeClassFile(classFile, cw)
    classFile
  }


  def visitLambdaFields(writer: ClassWriter, args: Map[String, Type]) = {
    for((name, tpe) <- args)
      writer.visitField(ACC_PRIVATE, name, GenerateClassFiles.getFieldSignature(tpe), null, null)
  }

  // We ain't never gon be real class, but de javas,she needs a comstruktar.
  private def visitLambdaConstructor(cw: ClassWriter, className: String, args: Map[String, Type]): Unit = {
    val mv = cw.visitMethod(ACC_PUBLIC, "<init>", GenerateClassFiles.getFunctionSignature(FunctionN(Unit, args.map(_._2).toSeq:_*), args.size), null, null)
    mv.visitVarInsn(ALOAD, 0)
    mv.visitMethodInsn(INVOKESPECIAL,
      "java/lang/Object",
      "<init>",
      "()V")
    // Now we write the fields.
    for(((arg, tpe), idx) <- args.zipWithIndex) {
      mv.visitVarInsn(ALOAD,0)
      val fs = GenerateClassFiles.getFieldSignature(tpe)
      // TODO - Here we are type dependent to avoid boxing.
      fs match {
        case "I" | "Z" => mv.visitIntInsn(ILOAD, idx + 1)
        case _ => mv.visitIntInsn(ALOAD, idx+1)
      }
      mv.visitFieldInsn(PUTFIELD, className, arg, fs)
    }
    mv.visitInsn(RETURN)
    mv.visitMaxs(1, 1)
    mv.visitEnd()
  }

  def visitLambdaApplyMethod(writer: ClassWriter, className: String, func: IdReferenceTyped, args: Map[String, Type], target: File) = {
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
    val (allArgTypes, returnType) = deconstructArgs(func.tpe)
    val unboundArgTypes = allArgTypes.drop(args.size)
    val mv = writer.visitMethod(ACC_PUBLIC, APPLY_METHOD_NAME, GenerateClassFiles.getMethodSignature(unboundArgTypes, returnType), null, null)
    // TODO - Figure out how to AST-ify all the fields + unboundArguments when generating the apply method...
    val boundArgAsts =
      for((name, tpe) <- args)
      yield IdReferenceTyped(name, TypeEnvironmentInfo(name, LocalField(className, name, tpe), tpe))
    val unboundArgAsts =
       for((tpe, idx) <- unboundArgTypes.zipWithIndex)
       yield {
         val name = s"unboundArg$idx"
         IdReferenceTyped(name, TypeEnvironmentInfo(name, Argument, tpe))
       }
    val defn = ApExprTyped(func, boundArgAsts.toSeq ++ unboundArgAsts, returnType)
    // TODO - Bind unbound argument names into local method state...
    val unboundArgLookup =
      unboundArgAsts.map(_.name).zipWithIndex.map { case(name, idx) => (name, idx+1)}.toMap
    val state = MethodWriterState(className, mv, unboundArgLookup, APPLY_METHOD_NAME, 0, target)
    System.err.println(s"Attempting to write lambda $className = $defn")
    MethodWriter.writeMethod(defn)(state)
  }

}
