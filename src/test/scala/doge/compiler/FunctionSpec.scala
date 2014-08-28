package doge.compiler


object FunctionSpec extends FullCompilerSpec {

  def is = s2"""
    This is a specification to check the runtime behavior of the DOGE language, built in function support.

    The built-in function support should
       curry application                                 $generateFunctionsAndCurriedApplications
       lift built-in partially applied functions         $liftBuiltIn
       lift lambda expressions                           $liftLambda
  """

  def liftLambda =
    """
      |WOW
      |foo
      |SO x
      |VERY Plus MUCH x 1! 1!
      |
      |WOW test
      |MUCH foo
      |  MANY x VERY Plus x 1!
      |!
    """.stripMargin must compileAndEvalTestAs(3)


  def liftBuiltIn =
    """|WOW lift
       |MUCH Plus 1!
       |
       |WOW test
       |MUCH lift 3!
       |""".stripMargin must compileAndEvalTestAs(4)

  def generateFunctionsAndCurriedApplications =
    """WOW
      |big
      |SO a b c d
      |MUCH Plus
      | MUCH Plus
      |   MUCH Plus c d!
      | b!
      |a!
      |
      |WOW
      |lifted
      |MUCH big !
      |
      |WOW
      |test
      |MUCH lifted 1 2 3 4!
    """.stripMargin must compileAndEvalTestAs(10)

}
