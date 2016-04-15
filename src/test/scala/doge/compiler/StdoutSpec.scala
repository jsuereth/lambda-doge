package doge.compiler

/**
 * Created by jsuereth on 8/19/14.
 */
object StdoutSpec extends FullCompilerSpec {

  def is = s2"""
    This is a specification to check the runtime behavior of the DOGE language for booleans

    The built-in Stdout should
       print integers               ${printInt and printlnInt}
       print booleans               ${printBool and printlnBool}
       print objects                ${printList and printlnList}
  """

  def printInt =
    """WOW
      |test
      |MUCH Print 1!
    """.stripMargin must compileAndEvalTestAs(null)

  def printBool =
    """WOW
      |test
      |MUCH Print true!
    """.stripMargin must compileAndEvalTestAs(null)


  def printList =
    """WOW
      |test
      |MUCH Print Nil!
    """.stripMargin must compileAndEvalTestAs(null)
  
  def printlnInt =
    """WOW
      |test
      |MUCH PrintLn 1!
    """.stripMargin must compileAndEvalTestAs(null)

  def printlnBool =
    """WOW
      |test
      |MUCH PrintLn true!
    """.stripMargin must compileAndEvalTestAs(null)


  def printlnList =
    """WOW
      |test
      |MUCH PrintLn Nil!
    """.stripMargin must compileAndEvalTestAs(null)
}

