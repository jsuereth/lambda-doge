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
import scala.util.parsing.input.{NoPosition, Position}

class TyperSpec extends Specification { def is = s2"""

    This is a specification to check the Parser of the DOGE language

    The Typer should unify
       simple qualified types                         $unifySimpleQualifiedTypes
       complex types                                  ${unifyComplexQualifiedTypes.pendingUntilFixed}

    The Typer should

      clear argument environment between lets         $handleMultiApplyBindIssue
      type boolean literals                           $typeBooleanLiterals
      type int literals                               $typeIntLiterals
      catch type errors                               $listError
      type built in references                        $typeReferences
      type partial application                        $handlePartialApply
      unify method application                        $unifyApplication
      unify let expressions                           $unifyLet
      bind let expressions inside modules             $bindLetExpressionsInModules
      fail on specified type mismatch                 $failOnSpecifiedTypeMisMatch
      type with specified types                       $useTypesSpecified
      type lambdas                                    $typeLambdas
                                                      """


  def unifySimpleQualifiedTypes = {
    val a = TypeSystem.newVariable
    val b = TypeSystem.newVariable
    val numA = TypeSystem.QualifiedType(TypeSystem.IsIn("Num"), a)
    val numB = TypeSystem.QualifiedType(TypeSystem.IsIn("Num"), b)
    (numA, numB) must unifyAs(numA)
  }

  def unifyComplexQualifiedTypes = {
    val a = TypeSystem.newVariable
    val b = TypeSystem.newVariable
    val numA = TypeSystem.QualifiedType(TypeSystem.IsIn("Num"), a)
    val eqB = TypeSystem.QualifiedType(TypeSystem.IsIn("Eq"), b)

    (numA, eqB) must unifyAs(numA)
  }

  def failOnSpecifiedTypeMisMatch = {
    LetExpr("bad", Some(Integer), Nil, ApExpr(IdReference("plus"), Seq(IntLiteral(1)))) must not(typeCheck)
  }

  def typeLambdas = {
    LambdaExpr(Seq("a", "b"), ApExpr(IdReference("plus"), Seq(IdReference("a"), IdReference("b")))) must
      typeAs(
        LambdaExprTyped(
          argNames = Seq("a", "b"),
          definition = ApExprTyped(
            name = plusReference,
            args = Seq(argReference("a", Integer), argReference("b", Integer)),
            tpe = Integer
          ),
          tpe = plusType
        )
      )
  }

  def useTypesSpecified = {
    LetExpr("specified", Some(Function(Integer, Integer)), Seq("a"), ApExpr(IdReference("is"), Seq(IdReference("a")))) must
      typeAs(
        LetExprTyped(
          name = "specified",
          argNames = Seq("a"),
          definition = ApExprTyped(
            name = IdReferenceTyped("is", TypeEnvironmentInfo("is", BuiltIn, Function(Integer, Integer))),
            args = Seq(IdReferenceTyped("a", TypeEnvironmentInfo("a", Argument, Integer))),
            tpe = Integer
          ),
          tpe = Function(Integer, Integer))
      )
  }

  def listError = {
    ApExpr(IdReference("cons"), Seq(IntLiteral(1), BoolLiteral(true))) must not(typeCheck)
  }

  def typeBooleanLiterals = {
    BoolLiteral(false) must typeAs(BoolLiteralTyped(false))
  }

  def typeIntLiterals = {
     IntLiteral(1) must typeAs(IntLiteralTyped(1))
  }

  def typeReferences =
     IdReference("is") must typeAs(IdReferenceTyped("is", defaultTypeEnv.lookup("is")))


  def unifyApplication =
     ApExpr(IdReference("plus"), Seq(ApExpr(IdReference("is"), Seq(IntLiteral(1))))) must
       typeAs(ApExprTyped(name = IdReferenceTyped("plus", TypeEnvironmentInfo("plus", BuiltIn, plusType)),
                         args = Seq(ApExprTyped(
                           IdReferenceTyped("is", TypeEnvironmentInfo(
                             "is",
                             BuiltIn,
                             Function(Integer, Integer)
                           )),

                           Seq(IntLiteralTyped(1)), Integer)),
                         tpe = Function(Integer, Integer)))

  def handlePartialApply =
    LetExpr("liftMe", None, Seq("a"), ApExpr(IdReference("plus"), Seq(IdReference("a")))) must
      typeAs(
        LetExprTyped(
          name = "liftMe",
          argNames = Seq("a"),
          definition = ApExprTyped(
                        name = IdReferenceTyped("plus", TypeEnvironmentInfo("plus", BuiltIn, plusType)),
                        args = Seq(IdReferenceTyped("a", TypeEnvironmentInfo("a", Argument, Integer))),
                        tpe = Function(Integer, Integer)
                       ),
          tpe = Function(Integer, Function(Integer, Integer)))
      )

  // TODO - handle nested apply

  def unifyLet =
     LetExpr("plusOne", None, Seq("x"),
       ApExpr(IdReference("plus"), Seq(IdReference("x"), IntLiteral(1)))
     ) must typeAs(
        LetExprTyped(
          name = "plusOne",
          argNames = Seq("x"),
          definition = ApExprTyped(
                          name = IdReferenceTyped("plus", TypeEnvironmentInfo("plus", BuiltIn, plusType)),
                          args = Seq(IdReferenceTyped("x", TypeEnvironmentInfo("x", Argument, Integer)), IntLiteralTyped(1)),
                          tpe = Integer),
          tpe = Function(Integer, Integer)
        )
     )

  def bindLetExpressionsInModules =
    Module(
      "test",
       Seq(
        LetExpr("liftMe", None, Seq("a"), ApExpr(IdReference("plus"), Seq(IdReference("a")))),
        LetExpr("liftMe2", None, Seq("b"), ApExpr(IdReference("liftMe"), Seq(IdReference("b"))))
       )
    ) must typeAs(
      ModuleTyped(
        "test",
        Seq(
          LetExprTyped(
            name = "liftMe",
            argNames = Seq("a"),
            definition = ApExprTyped(
              name = IdReferenceTyped("plus", TypeEnvironmentInfo("plus", BuiltIn, plusType)),
              args = Seq(IdReferenceTyped("a", TypeEnvironmentInfo("a", Argument, Integer))),
              tpe = Function(Integer, Integer)
            ),
            tpe = Function(Integer, Function(Integer, Integer))),
          LetExprTyped(
            name = "liftMe2",
            argNames = Seq("b"),
            definition = ApExprTyped(
              name = IdReferenceTyped("liftMe", TypeEnvironmentInfo("liftMe", StaticMethod("test", "liftMe", Seq(Integer), Function(Integer, Integer)), plusType)),
              args = Seq(IdReferenceTyped("b", TypeEnvironmentInfo("b", Argument, Integer))),
              tpe = Function(Integer, Integer)
            ),
            tpe = Function(Integer, Function(Integer, Integer)))
        )
      )
    )

  // Helpers
  val plusReference =
    IdReferenceTyped("plus", TypeEnvironmentInfo("plus", BuiltIn, plusType))
  def argReference(name: String, tpe: Type) =
    IdReferenceTyped(name, TypeEnvironmentInfo(name, Argument, tpe))

  def handleMultiApplyBindIssue =
    Module("test",
      Seq(
        LetExpr("Big", None, Seq("a", "b", "c", "d"),
          ApExpr(IdReference("plus"), Seq(
            IdReference("a"),
            ApExpr(IdReference("plus"),
              Seq(
                IdReference("b"),
                ApExpr(IdReference("plus"), Seq(IdReference("c"), IdReference("d")))
              )
            )
          ))
        ),
        LetExpr("other", None, Seq("a", "b"), ApExpr(IdReference("plus"), Seq(IdReference("a"), IdReference("b"))))
      )
    ) must typeAs(
      ModuleTyped(
        "test",
        Seq(
          LetExprTyped(
            name = "Big",
            argNames = Seq("a", "b", "c", "d"),
            definition =
              ApExprTyped(
                plusReference,
                Seq(
                  argReference("a", Integer),
                  ApExprTyped(
                    plusReference,
                    Seq(
                      argReference("b", Integer),
                      ApExprTyped(
                        plusReference,
                        Seq(argReference("c", Integer), argReference("d", Integer)),
                        Integer
                      )
                    ),
                    Integer
                  )
                ),
                Integer
              ),
            tpe = FunctionN(Integer, Integer, Integer, Integer, Integer)
          ),
          LetExprTyped(
            name = "other",
            argNames = Seq("a", "b"),
            definition = ApExprTyped(
              plusReference,
              Seq(
                argReference("a", Integer),
                argReference("b", Integer)
              ),
              Integer
            ),
            tpe = plusType
          )
        )
      )
    )


  def typeCheck: Matcher[DogeAst] = new Matcher[DogeAst] {
    def apply[S <: DogeAst](tree: Expectable[S]) = {
      try {
        Typer.typeTree(tree.value, defaultTypeEnv)
        // TODO - Compare types maybe needs to ignore variables....
        success("", tree)
      }
      catch {
        case t: TypeError => failure(s"Expected $tree got error: $t", tree)
        case t: SyntaxTypeError => failure(s"Expected $tree got error: $t", tree)
      }
    }
  }

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
        case t: SyntaxTypeError => failure(s"Expected $tree got error: $t", tree)
      }
    }
  }

  def unifyAs(tpe: Type) = new Matcher[(Type, Type)] {
    def apply[S <: (Type, Type)](tree: Expectable[S]) = {
      val (a, b) = tree.value
      val result = Typer.unify(a, b, NoPosition)(TyperEnvironment(defaultTypeEnv, Map.empty))._2
      if(result == tpe) success("", tree)
      else failure(s"Failed to unify ($a, $b), expected $tpe, found $result", tree)
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
    TypeEnv.dumbEnvironment(Seq(
      TypeEnvironmentInfo("is", BuiltIn, isType),
      TypeEnvironmentInfo("plus", BuiltIn, plusType)))
}