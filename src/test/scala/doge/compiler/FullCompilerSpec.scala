package doge.compiler

import java.io.{FileWriter, File}
import java.net.URLClassLoader
import java.util.Random

import doge.compiler.types.TypeError
import org.specs2.Specification
import org.specs2.matcher.{Expectable, Matcher}

import scala.reflect.ClassTag

trait FullCompilerSpec extends Specification {


  def compileAndEvalTestAs[A](value: A)(implicit mf: ClassTag[A]): Matcher[String] = new Matcher[String] {
    def apply[S <: String](program: Expectable[S]) = IO.withTemporaryDirectory { dir =>
      try {
        val result = evaluateTest(program.value)
        if(result != value) failure(s"Expected: $value, Found: $result in [${program.value}]", program)
        else  success("", program)
      } catch {
        case t: TypeError => failure(s"Type error $t in [${program.value}]", program)
        case t: NoTestMethodException => failure(s"Name not found: test in [${program.value}]", program)

      }
    }
  }



  private[this] def evaluateTest[A](program: String)(implicit mf: ClassTag[A]): A = IO.withTemporaryDirectory { dir =>
    Compiler.compile(program, dir, "test")
    ClassLoaderUtils.loadTestMethodResult(dir)
  }


}

class NoTestMethodException() extends RuntimeException() {}
object ClassLoaderUtils {
  def cl(dir: File): ClassLoader =
    new URLClassLoader(Array(dir.toURI.toURL))

  def loadClass(dir: File, name: String): Class[_] = {
    cl(dir).loadClass(name)
  }

  def loadTestMethodResult[A](dir: File)(implicit mf: ClassTag[A]): A = {
    val cls = loadClass(dir, "test")
    val mthd = cls.getDeclaredMethods.find(_.getName == "test").getOrElse(throw new NoTestMethodException())
    mthd.invoke(null).asInstanceOf[A]
  }
}

object IO {
  private val random = new Random(System.nanoTime)
  val temporaryDirectory = new File(System.getProperty("java.io.tmpdir"))

  def write(f: File, contents: String): Unit = {
    createDirectory(f.getParentFile)
    val out = new FileWriter(f)
    try out.write(contents)
    finally out.close()
  }

  /**
   * Creates a temporary directory and provides its location to the given function.  The directory
   * is deleted after the function returns.
   */
  def withTemporaryDirectory[T](action: File => T): T =
  {
    val dir = createTemporaryDirectory
    try { action(dir) }
    finally { dir.delete() }
  }

  /** Creates a directory in the default temporary directory with a name generated from a random integer. */
  def createTemporaryDirectory: File = createUniqueDirectory(temporaryDirectory)

  /** Creates a directory in `baseDirectory` with a name generated from a random integer */
  def createUniqueDirectory(baseDirectory: File): File =
  {
    def create(tries: Int): File =
    {
      if (tries > 3)
        sys.error("Could not create temporary directory.")
      else {
        val randomName = "sbt_" + java.lang.Integer.toHexString(random.nextInt)
        val f = new File(baseDirectory, randomName)

        try { createDirectory(f); f }
        catch { case e: Exception => create(tries + 1) }
      }
    }
    create(0)
  }

  /** Creates directory `dir` and all parent directories.  It tries to work around a race condition in `File.mkdirs()` by retrying up to a limit.*/
  def createDirectory(dir: File): Unit =
  {
    def failBase = "Could not create directory " + dir
    // Need a retry because mkdirs() has a race condition
    var tryCount = 0
    while (!dir.exists && !dir.mkdirs() && tryCount < 100) { tryCount += 1 }
    if (dir.isDirectory)
      ()
    else if (dir.exists) {
      sys.error(failBase + ": file exists and is not a directory.")
    } else
      sys.error(failBase)
  }
}