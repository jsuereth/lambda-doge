package doge.compiler.backend

import java.io.{File, StringWriter, PrintWriter, FileInputStream}

import org.objectweb.asm.ClassReader
import org.objectweb.asm.tree.{AbstractInsnNode, ClassNode}
import org.objectweb.asm.util.{TraceMethodVisitor, Textifier}

/**
 * Asm bytecode pretty printing.
 */
object AsmDebug {
  def prettyPrintClass(file: File): String = {
    val sw = new StringWriter()
    val in = new FileInputStream(file)
    val reader = new ClassReader(in)
    val classNode = new ClassNode()
    reader.accept(classNode,0)
    import scala.collection.JavaConverters._
    val methods = classNode.methods
    for(m <- methods.asScala){
      val sig = Option(m.signature) getOrElse m.desc
      val inList = m.instructions
      sw.append("-").append(m.name).append(" : ").append(sig).append("\n")
      for(insn <- inList.iterator().asScala){
        sw.append(insnToString(insn))
      }
    }
    sw.toString
  }

  def insnToString(insn: AbstractInsnNode ): String = {
    insn.accept(mp)
    val sw = new StringWriter()
    printer.print(new PrintWriter(sw))
    printer.getText.clear()
    sw.toString
  }

  private val printer = new Textifier()
  private val mp = new TraceMethodVisitor(printer)
}
