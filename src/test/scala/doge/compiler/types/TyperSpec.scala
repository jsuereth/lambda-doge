package doge.compiler.types

import doge.compiler.ast.ApExpr
import doge.compiler.ast.IdReference
import doge.compiler.ast.IntLiteral
import doge.compiler.ast.LetExpr
import doge.compiler.parser.DogeParser
import doge.compiler.parser.DogeParser._
import org.specs2._
import doge.compiler.ast._
import org.specs2.matcher.{Expectable, Matcher}
import TypeSystem._

class TyperSpec extends Specification { def is = s2"""

    This is a specification to check the Parser of the DOGE language

    The Typer should
      type  int literals                             $typeIntLiterals
      type  references                               $typeReferences
      unfiy application types                        $unifyApplication
      unify let types                                $unifyLet
                                                        """

  def typeIntLiterals = {
     IntLiteral(1) must typeAs(IntLiteralTyped(1))
  }

  def typeReferences =
     IdReference("is") must typeAs(IdReferenceTyped("is", defaultTypeEnv.lookup("is")))


  def unifyApplication =
     ApExpr(IdReference("plus"), Seq(ApExpr(IdReference("is"), Seq(IntLiteral(1))))) must
       typeAs(ApExprTyped(name = IdReferenceTyped("plus", plusType),
                         args = Seq(ApExprTyped(IdReferenceTyped("is", isType), Seq(IntLiteralTyped(1)), Integer)),
                         tpe = Function(Integer, Integer)))


  def unifyLet =
     LetExpr("plusOne", Nil, Seq("x"),
       ApExpr(IdReference("plus"), Seq(IdReference("x"), IntLiteral(1)))
     ) must typeAs(
        LetExprTyped(
          name = "plusOne",
          argNames = Seq("x"),
          definition = ApExprTyped(
                          name = IdReferenceTyped("plus", plusType),
                          args = Seq(IdReferenceTyped("x", Integer), IntLiteralTyped(1)),
                          tpe = Integer),
          tpe = Function(Integer, Integer)
        )
     )

  def typeAs(value: TypedAst): Matcher[DogeAst] = new Matcher[DogeAst] {
    def apply[S <: DogeAst](tree: Expectable[S]) = {
      try {
        val result = Typer.typeTree(tree.value, defaultTypeEnv)
        // TODO - Compare types maybe needs to ignore variables....
        if(result == value) success("", tree)
        else failure(s"Failed to properly type.\n\tExpected: $value\n\t     Got: $result", tree)
      }
      catch {
        case t: TypeError => failure(s"Expected $tree got error: $t", tree)
      }
    }
  }

  lazy val isType = {
    val a = newVariable
    Function(a, a)
  }
  lazy val plusType = {
    FunctionN(Integer, Integer, Integer)
  }

  lazy val defaultTypeEnv =
    TypeSystem.dumbEnvironment(Map(
      "is" -> isType,
      "plus" -> plusType

    ))
}