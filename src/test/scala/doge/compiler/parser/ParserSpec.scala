package doge.compiler.parser

import org.specs2._
import DogeParser._
import doge.compiler.ast._
import org.specs2.matcher.{Expectable, Matcher}

class ParserSpec extends Specification { def is = s2"""

    This is a specification to check the Parser of the DOGE language

    The Parser should
      parse int literals                             $parseIntLiterals
      parse identifiers                              $parseIdentifiers
      parse application                              $parseApplication
      parse let                                      $parseLet
                                                        """

  def parseIntLiterals = {
    "5" must parseAs(intLiteral, IntLiteral(5))
  }

  // TODO more rigorous testing.
  def parseIdentifiers = {
    "id" must parseAs(idRef, IdReference("id"))
  }

  def parseApplication = {
    ("VERY Big Doge!" must parseAs(apExpr, ApExpr(IdReference("Big"), Seq(IdReference("Doge"))))) and
    ("VERY Big 1!" must parseAs(apExpr, ApExpr(IdReference("Big"), Seq(IntLiteral(1)))))
  }

  def parseLet = {
    "WOW PlusOne SO numbers VERY Plus numbers 1!" must parseAs(letExpr, LetExpr(
      "PlusOne",
       types = Seq(),
       argNames = Seq("numbers"),
       definition = ApExpr(IdReference("Plus"), Seq(IdReference("numbers"), IntLiteral(1)))
    ))
  }


  def parseAs[A](expr: Parser[A], value: A): Matcher[String] = new Matcher[String] {
    def apply[S <: String](s: Expectable[S]) = {
      parseAll(expr, s.value) match {
        case Success(`value`, _) => success("successfully parsed", s)
        case Success(wrong, _) => failure(s"Unexpected parser result\n\t   Found: $wrong\n\tExpected: $value", s)
        case result => failure(s"Failed to parse expressions, got $result", s)
      }
    }
  }
}