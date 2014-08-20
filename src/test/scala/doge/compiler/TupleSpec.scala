package doge.compiler


object TupleSpec extends FullCompilerSpec {

  def is = s2"""
    This is a specification to check the runtime behavior of the DOGE language, built in tuples.

    The built-in Tuple should
       return the first element from fst                 $fst
       return the second element rom snd                 $snd
  """


  def fst =
    """WOW
      |t
      |MUCH tuple2 1 true!
      |
      |WOW
      |test
      |MUCH fst t!
    """.stripMargin must compileAndEvalTestAs(1)


  def snd =
    """WOW
      |t
      |MUCH tuple2 1 true!
      |
      |WOW
      |test
      |MUCH snd t!
    """.stripMargin must compileAndEvalTestAs(true)
}
