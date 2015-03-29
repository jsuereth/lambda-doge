package doge.compiler.classloader

import java.io.{FileInputStream, File, InputStream}
import java.net.{URLClassLoader, URL}
import java.util.jar.JarFile

import org.objectweb.asm.{ClassReader, ClassVisitor}

/** Abstraction over Java class files. */
trait ClassFinder {
  /** Looks for a class with the given name, and returns something which can "visit" the class using ASM. */
  def find(name: String): Option[VisitableClass]
}
object ClassFinder {
  /** Turns a java class name into a filename that should load the class. */
  def javaNameToPath(className: String): String = {
    // TODO - Handle nested classes?
    className.replaceAllLiterally(".", "/") + ".class"
  }

  /** Uses the current jvm properties to find the boot classpath and create somethign which can
    * load JVM class files.
    * @return
    */
  def bootClasspath: ClassFinder = {
    val urls =
      for {
        f <- sys.props("sun.boot.class.path").split(":").map(new File(_))
        if f.exists() && (f.isDirectory || (f.getName endsWith ".jar"))
      } yield
        if(f.getName endsWith ".jar") new JarClassFinder(new JarFile(f))
        else new DirClassFinder(f)
    new ClasspathClassFinder(urls)
  }

  // TODO - with parents, or not...
  def fromClassLoader(cl: ClassLoader): ClassFinder = {
    val parents: Seq[ClassFinder] =
      if(cl.getParent == null) Nil
      else fromClassLoader(cl.getParent) :: Nil
    cl match {
      case url: URLClassLoader =>
        val finders: Seq[ClassFinder] =
          for {
            url <- url.getURLs
            // TODO - some may not be files
            f = new java.io.File(url.toURI)
            if (f.getName endsWith ".jar") || f.isDirectory
          } yield
            if(f.isDirectory) new DirClassFinder(f)
            else new JarClassFinder(new JarFile(f))
        new ClasspathClassFinder(finders ++ parents)
      case other =>
        sys.error(s"Unable to handle classloader to classpath:  $other")
    }
  }
}


/** A trait which allows visiting a .class file using ASM.  The class could be in memory, on disk or in a JAR. */
trait VisitableClass {
  /** Visits a given class with a class visitor.  This will open/close streams as necessary. */
  def visit(cv: ClassVisitor, flags: Int): Unit
  /** Loads the raw symbols of the class.  Note, this does not cache anything it loads. */
  def rawSymbols: Option[JavaClassInfo] = {
    val visitor = new ClassSymbolReader
    visit(visitor, 0)
    visitor.classInfo
  }
}
/** A visitable class which creates an input stream and closes it on every visit. */
class InputStreamVisitableClass(name: String, in: Function0[InputStream]) extends VisitableClass {
  /** Visits a given class with a class visitor.  This will open/close streams as necessary. */
  override def visit(cv: ClassVisitor, flags: Int): Unit = {
    val input = in()
    try {
      val cr = new ClassReader(input)
      cr.accept(cv, flags);
    } finally input.close()
  }
  override def toString = name
}
/** A class finder which looks for classes in a JAR. */
class JarClassFinder(jar: JarFile) extends ClassFinder {
  /** Looks for a class with the given name, and returns something which can "visit" the class using ASM. */
  override def find(name: String): Option[VisitableClass] = {
    Option(jar.getJarEntry(ClassFinder.javaNameToPath(name))) map { entry =>
      new InputStreamVisitableClass(s"jar:${jar.getName},class:$name", () => jar.getInputStream(entry))
    }
  }
}
/** A class finder which looks for classes in a directory. */
class DirClassFinder(dir: File) extends ClassFinder {
  /** Looks for a class with the given name, and returns something which can "visit" the class using ASM. */
  override def find(name: String): Option[VisitableClass] = {
    val path = ClassFinder.javaNameToPath(name)
    val cf = new File(dir, path)
    if(cf.exists) Some(new InputStreamVisitableClass(s"file:$path",() => new FileInputStream(cf)))
    else None
  }
}
/** A clas finder which mimics the classpath.  Will return the *first* class it finds in the list of finders. */
class ClasspathClassFinder(cp: Seq[ClassFinder]) extends ClassFinder {
  /** Looks for a class with the given name, and returns something which can "visit" the class using ASM. */
  override def find(name: String): Option[VisitableClass] = {
    val i = cp.iterator.flatMap(_.find(name).toSeq)
    if(i.hasNext) Some(i.next)
    else None
  }
}