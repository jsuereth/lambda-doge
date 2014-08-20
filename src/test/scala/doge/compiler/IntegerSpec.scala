package doge.compiler


object IntegerSpec extends FullCompilerSpec {

  def is = s2"""
    This is a specification to check the runtime behavior of the DOGE language for integers

    The built-in Integer should
       sum with the Plus method                    $plus
       subtract with the Minus method              $minus
       multiply with the Multiply method           $mult
       divide with the Divide method               $div
  """


  def plus =
    """WOW
      |test
      |MUCH Plus 4 5!
    """.stripMargin must compileAndEvalTestAs(9)


  def minus =
    """WOW
      |test
      |MUCH Minus 5 4!
    """.stripMargin must compileAndEvalTestAs(1)


  def mult =
    """WOW
      |test
      |MUCH Multiply 5 4!
    """.stripMargin must compileAndEvalTestAs(20)

  def div =
    """WOW
      |test
      |MUCH Divide 20 4!
    """.stripMargin must compileAndEvalTestAs(5)
}

