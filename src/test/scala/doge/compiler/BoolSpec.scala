package doge.compiler

/**
 * Created by jsuereth on 8/19/14.
 */
object BoolSpec extends FullCompilerSpec {

  def is = s2"""
    This is a specification to check the runtime behavior of the DOGE language for booleans

    The built-in bool should
       fork the true branch of if method                $ifTrue
       fork the false branch of if method               $ifFalse
  """


  def ifFalse =
    """WOW
      |cat
      |MUCH IS false!
      |
      |WOW
      |test
      |MUCH ifs cat 4 5!
    """.stripMargin must compileAndEvalTestAs(5)



  def ifTrue =
    """WOW
      |cat
      |MUCH IS true!
      |
      |WOW
      |test
      |MUCH ifs cat 4 5!
    """.stripMargin must compileAndEvalTestAs(4)
}

