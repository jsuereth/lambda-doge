package doge.compiler

object JavaSpec extends FullCompilerSpec {
  def is = s2"""
    This is the specification for Java-language interaction with the Lambda-Doge language.


    java.lang.String in lambda-doge should
      be constructed and returned in $returnJavaString
      call instance methods in $callJavaStringLength

    java.lang.Boolean in lambda-doge should
      call static methods in $callStaticBooleanMethods
  """


  def returnJavaString = {
    """
      |WOW
      |test
      |MUCH java.lang.String#new !
    """.stripMargin must compileAndEvalTestAs("")
  }
  def callJavaStringLength = {
    """
      |WOW
      |test
      |MUCH java.lang.String#length
      |VERY java.lang.String#new
      |!!
    """.stripMargin must compileAndEvalTestAs(0)
  }

  def callStaticBooleanMethods = {
    """
      |WOW
      |test
      |MUCH java.lang.Boolean#compare
      |true true
      |!
    """.stripMargin must compileAndEvalTestAs(0)
  }
}
