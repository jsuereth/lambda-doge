package doge.compiler.classloader

import doge.compiler.types.TypeSystem
import doge.compiler.types.TypeSystem.Type
import org.objectweb.asm.signature.{SignatureVisitor, SignatureReader}
import org.objectweb.asm.{MethodVisitor, Opcodes, ClassVisitor, FieldVisitor}


// Temporary structures we use to, eventually, construct symbols.
case class JavaClassRef(name: String) {
  override def toString = name
}
case class RawJavaClassSymbol(name: String, superClass: Option[JavaClassRef], interfaces: Seq[JavaClassRef]) {
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
    baseClassSym = Some(RawJavaClassSymbol(jvmClassToJavaClassName(name), superCls, interfaces.toSeq map jvmClassToJavaClassName map JavaClassRef.apply))
  }

  override def visitOuterClass(owner: String, name: String, desc: String): Unit =
    sys.error("Inner classes not supported!")

  override def visitField(access: Int, name: String, desc: String, signature: String, initialValue: Any): FieldVisitor = {
    if((Opcodes.ACC_PUBLIC & access) > 0) {
      val static = (Opcodes.ACC_STATIC & access) > 0
      val sig = if (signature == null) desc else signature
      val sr = new SignatureReader(sig)
      val sc = new SignatureConverter
      sr.accept(sc)
      fields ::= JavaField(name, desc, sc.tpe.get, static)
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
      val sc = new SignatureConverter
      sr.accept(sc)
      if (ClassSymbolReader.isConstructor(name)) {
        constructors +:= JavaConstructor(desc, sc.tpe.get, sc.arity.get)
      } else {
        val static = ((Opcodes.ACC_STATIC & access) > 0)
        (sc.tpe, sc.arity) match {
          case (Some(tpe), Some(arity)) =>
            methods +:= JavaMethod(name, desc, tpe, arity, static)
          case _ =>
            sys.error(s"Error parsing method $name : $sig, static = $static")
        }

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

/** Converts a JDK signature into a type in our typesystem. */
class SignatureConverter extends SignatureVisitor(Opcodes.ASM5) {

  var tpe: Option[Type] = None
  var state: SigState = BaseState
  var arity: Option[Int] = None

  sealed trait SigState {
    def handleType(t: Type): Unit
    def previous: SigState
  }
  object BaseState extends SigState {
    def handleType(t: Type): Unit = {
      tpe = Some(t)
      // TODO - is this correct?
      state = state.previous
    }
    override def previous = BaseState
    override def toString = "BaseState"
  }
  case class FunctionArgState(args: Seq[Type], val previous: SigState) extends SigState {
    override def handleType(t: Type): Unit = {
      state = new FunctionArgState(args :+ t, previous)
    }
    override def toString = s"FunctionArgState($args) -> $previous"
  }
  class FunctionState(params: Seq[Type], val previous: SigState) extends SigState {
    def handleType(t: Type): Unit = tpe = Some(TypeSystem.FunctionN(t, params:_*))
    override def toString = s"FunctionState(${params}) -> $previous"
  }
  class ArrayState(val previous: SigState) extends SigState {
    def handleType(t: Type): Unit = previous.handleType(TypeSystem.ArrayType(t))
    override def toString = s"ArrayState -> $previous"
  }

  override def visitClassType (name: String): Unit = {
    // TODO - global help function for this.
    val realName = name.replaceAllLiterally("/", ".")
    realName match {
      case "java.lang.String" => state.handleType(TypeSystem.String)
      case _ =>
        val tp = TypeSystem.QualifiedType(TypeSystem.IsIn(realName), TypeSystem.newVariable)
        //if(realName == s"java.lang.Object") System.err.println(s"Creating qualified type [$tp] for java.lang.Object, state = ${state}")
        state.handleType(tp)
    }
  }

  override def visitParameterType(): SignatureVisitor = {
    arity = arity.map(_+1) orElse Some(1)
    state =
      state match {
        case x: FunctionArgState => x
        case other => new FunctionArgState(Nil, state)
      }
    this
  }

  override def visitReturnType(): SignatureVisitor = {
    state = state match {
      case x: FunctionArgState => new FunctionState(x.args, x.previous)
      case _ =>
        arity = Some(0)
        new FunctionState(tpe.toSeq, state.previous)
    }
    this
  }

  override def visitFormalTypeParameter(name: String): Unit = {
    sys.error(s"Unable to handle paramater named $name, Java generics are not supported yet!")
  }

  override def visitTypeArgument(): Unit = {
    // Here we've encountered an unbound generic.
    state.handleType(TypeSystem.newVariable)
  }

  override def visitTypeArgument(wildcard: Char): SignatureVisitor = {
    // This is called when we're about to read the generic signature of a type argument.
    // i.e. the type has some kind of bounds on it.   This is not supported yet.
    System.err.println(s"Wildcard argumnet $wildcard!, state = $state")
    wildcard match {
      case '=' => this
      case '+' => this
      case _ => sys.error(s"Unable to handle java generics (? super T)!")
    }

  }

  override def visitArrayType(): SignatureVisitor = {
    state = new ArrayState(state)
    this
  }

  override def visitBaseType (descriptor: Char): Unit = {
    val nextTpe =
    descriptor match {
      case 'I' => TypeSystem.Integer
      case 'V' => TypeSystem.Unit
      case 'Z' => TypeSystem.Bool
        // TODO - The rest of these built in
      case 'B' => TypeSystem.Simple("Byte")
      case 'C' => TypeSystem.Simple("Char")
      case 'D' => TypeSystem.Simple("Double")
      case 'F' => TypeSystem.Simple("Float")
      case 'J' => TypeSystem.Simple("Long")
      case 'S' => TypeSystem.Simple("Short")
      case _ => TypeSystem.TypeConstructor(descriptor.toString, Nil)
    }
    state.handleType(nextTpe)
  }
  override def visitEnd(): Unit = {
    //System.err.println(s"Visiting end, $state")
    state match {
      case x: FunctionArgState => // Ignore
      case _ => state = state.previous
    }
  }
}
