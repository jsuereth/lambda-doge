package doge.compiler

object ListSpec extends FullCompilerSpec {

  def is = s2"""
    This is a specification to check the runtime behavior of the DOGE language

    The built-in List should
       return the first element from hd                 $popHead
       return the tail of a list                        $returnTail
  """


  def popHead =
    """WOW
      |list
      |MUCH cons 1 Nil!
      |
      |WOW
      |test
      |MUCH hd list!
    """.stripMargin must compileAndEvalTestAs(1)


  def returnTail =
     """WOW
       |list
       |MUCH cons 2
       |VERY cons 1 Nil!!
       |
       |WOW
       |test
       |MUCH tl list!
     """.stripMargin must compileAndEvalTestAs({
       val tmp = new java.util.concurrent.CopyOnWriteArrayList[AnyRef]()
       tmp.add(new java.lang.Integer(1))
       tmp
     })
}
