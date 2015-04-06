package doge.compiler.classloader

import doge.compiler.types.TypeSystem
import doge.compiler.types.TypeSystem.Type
import org.objectweb.asm.signature.{SignatureVisitor, SignatureReader}
import org.objectweb.asm.{MethodVisitor, Opcodes, ClassVisitor, FieldVisitor}

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}


// Temporary structures we use to, eventually, construct symbols.
case class JavaClassRef(name: String) {
  override def toString = name
}
case class RawJavaClassSymbol(name: String, superClass: Option[JavaClassRef], interfaces: Seq[JavaClassRef], isInterface: Boolean, isAbstract: Boolean) {
  override def toString = s"$name${superClass.map(" extends "+).getOrElse("")}${if(interfaces.isEmpty) "" else interfaces.mkString(" with ", " with ", "")}"
}
sealed trait ClassMember
case class JavaMethod(name: String, jvmDesc: String, tpe: Type, numArgs: Int, static: Boolean) extends ClassMember {
  override def toString = s"${if(static) "static" else "member"} $name: $tpe"
}
case class JavaConstructor(jvmDesc: String, tpe: Type, numArgs: Int) extends ClassMember {
  override def toString = s"constr this: $tpe"
}
case class JavaField(name: String, jvmDesc: String, tpe: Type, static: Boolean) extends ClassMember {
  override def toString = s"${if(static) "static" else "member"} ${name}: $tpe"
}

case class JavaClassInfo(cls: RawJavaClassSymbol, members: Seq[ClassMember]) {
  def constructors: Seq[JavaConstructor] = members collect {
    case x: JavaConstructor => x
  }
  def fields: Seq[JavaField] = members collect { case x: JavaField => x }
  def methods: Seq[JavaMethod] = members collect { case x: JavaMethod => x}
  override def toString =
  s"""${cls}
     | * Constructors
     |${constructors.mkString("   - ", "\n   - ", "")}
     | * Methods
     |${methods.sortBy(m => s"${m.static}${m.name}").mkString("   - ", "\n   - ", "")}
     | * Fields
     |${fields.mkString("   - ", "\n   - ", "")}
   """.stripMargin
}

object ClassSymbolReader {
  val CONSTRUCTOR_NAME="<init>"
  val STATIC_INIT_NAME="<clinit>"

  def isConstructor(nme: String): Boolean= nme == CONSTRUCTOR_NAME
}
/**
 * Reads the raw symbols within a classfile.
 */
class ClassSymbolReader extends ClassVisitor(Opcodes.ASM5) {

  var classInfo: Option[JavaClassInfo] = None
  private var baseClassSym: Option[RawJavaClassSymbol] = None
  private var methods: List[JavaMethod] = Nil
  private var constructors: List[JavaConstructor] = Nil
  private var fields: List[JavaField] = Nil

  private def jvmClassToJavaClassName(nme: String): String = nme.replaceAllLiterally("/", ".")

  override def visit (version: Int, access: Int, name: String, signature: String, superName: String, interfaces: Array[String]): Unit = {
    // TODO - Create the symbols representing this guys class hierarchy.
    // TODO - We need to remember the generic type variables for when we cosntruct symbols...
    //if(signature != null) sys.error(s"We do not handle java generics yet. Found signature: $signature")
    // TODO - The signature tells us the generics used in our parents, too.

    val superCls = Option(superName) map jvmClassToJavaClassName map JavaClassRef.apply

    val isInterface = (Opcodes.ACC_INTERFACE & access) > 0
    val isAbstract = (Opcodes.ACC_ABSTRACT & access) > 0
    baseClassSym = Some(RawJavaClassSymbol(jvmClassToJavaClassName(name), superCls, interfaces.toSeq map jvmClassToJavaClassName map JavaClassRef.apply, isInterface, isAbstract))
  }

  override def visitOuterClass(owner: String, name: String, desc: String): Unit =
    sys.error("Inner classes not supported!")

  override def visitField(access: Int, name: String, desc: String, signature: String, initialValue: Any): FieldVisitor = {
    if((Opcodes.ACC_PUBLIC & access) > 0) {
      val static = (Opcodes.ACC_STATIC & access) > 0
      val sig = if (signature == null) desc else signature
      val sr = new SignatureReader(sig)
      val sc = new TypeSignatureVisitor
      sr.accept(sc)
      // TODO - not so ugly here
      val tpe = sc.tpe.value.get match {
        case Success(t) => t
          // TODO - When we support java generics, this should go away.
        // Here we should issue a warning about dropping to the erased types!
        case Failure(t) if signature != null =>
          val sr = new SignatureReader(desc)
          val sv = new TypeSignatureVisitor
          sr.accept(sv)
          sv.tpe.value.get.get
        case Failure(t) => throw t
      }
      fields ::= JavaField(name, desc, tpe, static)
    }
    null
  }

  override def visitMethod(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]): MethodVisitor = {
    // Only grab public fields that are not synthetically generated.
    if (((Opcodes.ACC_PUBLIC & access) > 0 ) && !((Opcodes.ACC_SYNCHRONIZED & access) > 0)) {
      // TODO - Any other method names we should ignore?
      if (name == ClassSymbolReader.STATIC_INIT_NAME) return null
      val sig = if (signature == null) desc else signature
      val sr = new SignatureReader(sig)
      val sc = new MethodSignatureVisitor
      try sr.accept(sc)
      catch {
        case t: Throwable =>
          System.err.println(s"Failure on ($desc)($signature)")
          throw t
      }
      val MethodSymbol(tpe, arity) = sc.sym.value.get match {
        case Success(ms) => ms
        case Failure(t) if signature != null =>
          // TODO - We shouldn't used the erased signature, but we don't support java generics, completely, yet.
          // Here we should issue a warning about dropping to the erased types!
          val sr2 = new SignatureReader(desc)
          val sv2 = new MethodSignatureVisitor
          try sr2.accept(sv2)
          catch {
            case t: Throwable =>
              System.err.println(s"Failure on ($desc)($signature)")
              throw t

          }
          sv2.sym.value.get.get
        case Failure(t) => throw t
      }
      if (ClassSymbolReader.isConstructor(name)) {
        constructors +:= JavaConstructor(desc, tpe, arity)
      } else {
        val static = ((Opcodes.ACC_STATIC & access) > 0)
        methods +:= JavaMethod(name, desc, tpe, arity, static)
      }
    }
    null
  }

  override def visitEnd(): Unit = {
    classInfo =
      baseClassSym map { sym =>
        JavaClassInfo(sym, constructors ++ methods ++ fields)
      }
  }
}


