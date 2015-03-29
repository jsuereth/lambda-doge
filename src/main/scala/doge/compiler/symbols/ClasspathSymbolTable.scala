package doge.compiler.symbols

import doge.compiler.classloader._
import doge.compiler.types.TypeSystem
import doge.compiler.types.TypeSystem.Type



object ClasspathSymbolTable {
  // For testing.
  def boot = new ClasspathSymbolTable(ClassFinder.bootClasspath)
}
/**
 * A symbol table which can load all the symbols on demand from java classes.
 */
class ClasspathSymbolTable(cf: ClassFinder) extends SymbolTable{
  /** Uses the given name to lookup a symbol in the symbol table. */
  override def lookup(name: String): Option[DogeSymbol] = {
    // TODO - we should have some kind of pattern for ripping the name into constituent parts for JVM access.
    // e.g. java.lang.String#length -> references the length method on java.lang.String
    for {
      cl <- cf.find(name)
    } yield new MyJavaClassSymbol(name, cl)
  }


  /** Represents a java class symbol when we can read the class file. */
  class MyJavaClassSymbol(val name: String, cls: VisitableClass) extends JavaClassSymbol {
    private lazy val info: JavaClassInfo = {
      cls.rawSymbols match {
        case Some(sym) => sym
        case None => sys.error(s"Failed to load class: $name")
      }
    }
    override lazy val interfaces: Seq[JavaClassSymbol] = {
      // TODO - Can we guarantee we always return JavaClassSymbols?
      info.cls.interfaces.map { i =>
        lookup(i.name) match {
          case Some(x: JavaClassSymbol) => x
            // TODO - return a stub for now, and hope we don't need to use this guy.
          case _ => StubJavaClassSymbol(i.name, true)
        }
      }
    }
    // TODO - Implement these.
    override def isInterface: Boolean = ??? //info.cls.isInterface
    override def isAbstract: Boolean = ??? //info.cls.isAbstract
    override lazy val parentClass: Option[JavaClassSymbol] =
      info.cls.superClass map { i => StubJavaClassSymbol(i.name, false) }

    override lazy val methods: Seq[JavaMethodSymbol] = {
      info.methods map { m =>
        MyJavaMethodSymbol(m, this)
      }
    }
    override lazy val constructors: Seq[JavaConstructorSymbol] = {
      info.constructors map { c =>
        MyJavaConstructorSymbol(c, this)
      }
    }
    override lazy val fields: Seq[JavaFieldSymbol] = {
      info.fields map { f => MyJavaFieldSymbol(f, this)}
    }
    override def tpe: Type = {
      // TODO - Handle java generics...
      TypeSystem.Simple(name)
    }
    override def toString = s"Classfile ${name}"
  }

  case class MyJavaMethodSymbol(info: JavaMethod, owner: JavaClassSymbol) extends JavaMethodSymbol {
    override def name: String = info.name
    override def arity: Int = info.numArgs
    override def tpe: Type =
      if(isStatic) info.tpe
      else TypeSystem.Function(owner.tpe, info.tpe)
    override def isStatic: Boolean = info.static
    override def toString: String = s"${if(isStatic) "func" else "mthd"} $name: $tpe"
  }

  /** A symbol representing java constructors as functions which take arguments and return the constructed thing. */
  case class MyJavaConstructorSymbol(info: JavaConstructor, owner: JavaClassSymbol) extends JavaConstructorSymbol {
    override def arity: Int = info.numArgs
    override lazy val tpe: Type = {
      // we need to replace the last function type of "Unit" with owner.tpe. THis converts from raw JVM signature to
      // what DOGE sees.
      def fix(tpe: Type, arity: Int): Type = tpe match {
        case TypeSystem.Function(l, r) if arity > 0 => TypeSystem.Function(l, fix(r, arity - 1))
        case TypeSystem.Function(l, r) => TypeSystem.FunctionN(l, owner.tpe)
        case x => owner.tpe
          // ANything else is an error!
      }
      fix(info.tpe, info.numArgs)
    }
    override def toString: String = s"func $tpe"
  }

  /** Represents a java field. */
  case class MyJavaFieldSymbol(field: JavaField, owner: JavaClassSymbol) extends JavaFieldSymbol {
    override def name: String = field.name
    override def isStatic: Boolean = field.static
    override lazy val tpe: Type = {
      if(isStatic) field.tpe
      else TypeSystem.Function(owner.tpe, field.tpe)
    }
    override def toString: String = s"${if(isStatic) "value" else "field" } $name: $tpe"
  }

  /** This symbol is used to stub for classes we do not want to read UNLESS absolutely necessary.
    *
    * Reading the class file may also fail, issuing an exception at that point in the program.
    */
  case class StubJavaClassSymbol(name: String, isInterface: Boolean) extends JavaClassSymbol {
    private lazy val loaded: JavaClassSymbol = {
      lookup(name) match {
        case Some(x: JavaClassSymbol) => x
        case _ => sys.error(s"Unable to find $name on the classpath!")
      }
    }
    override def tpe: Type = loaded.tpe
    override def interfaces: Seq[JavaClassSymbol] = loaded.interfaces
    override def parentClass: Option[JavaClassSymbol] = loaded.parentClass
    override def methods: Seq[JavaMethodSymbol] = loaded.methods
    override def constructors: Seq[JavaConstructorSymbol] = loaded.constructors
    override def isAbstract: Boolean =
      if(isInterface) true else loaded.isAbstract
    override def fields: Seq[JavaFieldSymbol] = loaded.fields
    override def toString = s"StubClass $name"
  }


}
