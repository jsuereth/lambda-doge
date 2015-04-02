package doge.compiler.symbols

import doge.compiler.classloader._
import doge.compiler.types.TypeSystem
import doge.compiler.types.TypeSystem.Type

import scala.util.Try


object ClasspathSymbolTable {
  // For testing.
  def boot = new ClasspathSymbolTable(ClassFinder.bootClasspath)
}

object JavaMethodName {
  def unapply(in: String): Option[(String, String)] = {
    if(in.contains("#")) {
      in.split("#") match {
        case Array(cls, mthd) => Some((cls, mthd))
        case _ => None
      }
    } else None
  }
  def apply(cls: String, mthd: String): String = s"${cls}#${mthd}"
}
object JavaConstructorName {
  def unapply(in: String): Option[String] = {
    in match {
      case JavaMethodName(cls, "new") => Some(cls)
      case _ => None
    }
  }
  def apply(cls: String): String = s"${cls}#new"
}

/**
 * A symbol table which can load all the symbols on demand from java classes.
 */
class ClasspathSymbolTable(cf: ClassFinder) extends SymbolTable {
  import com.google.common.cache.{LoadingCache,CacheBuilder, CacheLoader}

  // A cache for loaded symbols so we avoid loading the same classfile multiple times, if possible.
  private val symCache: LoadingCache[String, DogeSymbol] =
    CacheBuilder.newBuilder().maximumSize(1000L).build(
      new CacheLoader[String, DogeSymbol] {
        override def load(key: String): DogeSymbol = {
          lookupInternal(key).getOrElse(throw new IllegalArgumentException(s"$key is not a symbol in the java classpath."))
        }
      }
    )
  /** Uses the given name to lookup a symbol in the symbol table. */
  override def lookup(name: String): Option[DogeSymbol] = Try(symCache.get(name)).toOption


  private def lookupInternal(name: String): Option[DogeSymbol] = {

    def lookupClass(cls: String): Option[JavaClassSymbol] = {
      for {
        cl <- cf.find(cls)
      } yield new MyJavaClassSymbol(cls, cl)
    }
    // TODO - We need some kind of mechanism for picking which method in the sea of overloaded methods to
    //       promote in doge.  "First" is a poor option.
    name match {
      case JavaConstructorName(className) =>
        lookupClass(className).map { c =>
          c.constructors.sortBy(_.arity) match {
            case Seq(cons) => cons
            case Seq() => throw new IllegalArgumentException(s"No constructor found for $c")
            case Seq(cons, _*) =>
              // TODO - real warning system
              System.err.println(s"Warning:  Class $className has more than one constructor.  Picking the first one we see: $c")
              cons
          }
        }
      case JavaMethodName(className, mthd) =>
        lookupClass(className) map { c =>
          // TODO - Maybe we need to look up the parent hierarchy?
          c.methods collect { case m if m.name == mthd => m } sortBy { _.arity } match {
            case Seq(m) => m
            case Seq() => throw new IllegalArgumentException(s"No method/func $mthd found in class $className")
            case Seq(mthd, _*) =>
              System.err.println(s"Warning:  Method $name is overloaded! picking the first option we find ($mthd) ")
              mthd
          }
        }
      case _ => lookupClass(name)
    }
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
    override def name: String = s"${owner.name}#new"
    override def arity: Int = info.numArgs
    override lazy val tpe: Type = {
      // we need to replace the last function type of "Unit" with owner.tpe. THis converts from raw JVM signature to
      // what DOGE sees.
      def fix(tpe: Type, arity: Int): Type = tpe match {
        case TypeSystem.Function(l, r) if arity > 0 => TypeSystem.Function(l, fix(r, arity - 1))
        case TypeSystem.Function(l, r) => TypeSystem.FunctionN(l, owner.tpe)
          // TODO - THis may be wrong, uneeded...
        case x => owner.tpe
          // ANything else is an error!
      }
      fix(info.tpe, info.numArgs)
    }
    override def toString: String = s"func $tpe"
  }

  /** Represents a java field. */
  case class MyJavaFieldSymbol(field: JavaField, owner: JavaClassSymbol) extends JavaFieldSymbol {
    override def name: String = s"${owner.name}#${field.name}"
    override def isStatic: Boolean = field.static
    override lazy val tpe: Type = {
      if(isStatic) field.tpe
      else TypeSystem.Function(owner.tpe, field.tpe)
    }
    override def toString: String = s"${if(isStatic) "value" else "field" } ${field.name}: $tpe"
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
