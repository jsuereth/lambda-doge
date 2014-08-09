package doge.compiler.interpreter

import doge.compiler.ast._

object Interpreter {


  def interpret(ast: Seq[DogeAst]): Any = {
    val state = DogeState()
    val finalState = ast.foldLeft(state)({ case(s, expr) =>
      val (next, _) = interpretNext(expr, s)
      next
    })
    finalState.get("main") match {
      case f: DogeLambda => f.apply(finalState, Nil)
      case v: DogeValue => v.toJvm
    }
  }

  def interpretNext(expr: DogeAst, state: DogeState): (DogeState, DogeValue) = {
    //println(s"Evaluating $expr in $state")
    expr match {
      case IdReference(name) => (state, state.get(name))
      case IntLiteral(value) => (state, DogeInt(value))
      case ApExpr(name, values) =>
        state.get(name.name) match {
          case f: DogeLambda =>
            val zero: (DogeState, Seq[DogeValue]) = (state, Nil)
            val (nextState, args) = values.foldLeft(zero) { case ((state, args), nextArgAst) =>
              val (nextState, nextArg) = interpretNext(nextArgAst, state)
              (nextState, args :+ nextArg)
            }
            val result = f(nextState, args) match {
              case x: Thunk => x.apply(nextState, Nil)
              case y => y
            }
            (nextState, result)
          case v: DogeValue if values.isEmpty => (state, v)
          case _ => sys.error(s"Cannot evaluate $name($values) in $state")
        }

      case LetExpr(name, _, argNames, definition) =>
        val func = DynamicDogeFunc(argNames, definition)
        val value = func match {
          case x: Thunk => x.apply(state, Nil)
          case y => y
        }
        (state.put(name, value), value)
    }
  }



   class DogeState(private val lets: Map[String, DogeValue]) {
    def get(name: String): DogeValue = lets.get(name).getOrElse {
      sys.error(s"$name not found in $this")
    }
    def put(name: String, value: DogeValue): DogeState =
      DogeState(lets + ((name, value)))


    def join(other: DogeState): DogeState =
      DogeState(other.lets ++ lets)

     override def toString = s"State {\n\t${lets.map({case (x,y) => s"lex $x = $y"}).mkString("\n\t")}\n}"

  }
  object DogeState {
    def apply(lets: Map[String, DogeValue]) = new DogeState(lets)
    def apply() =
      new DogeState(
        builtIns
      )

    private val builtIns: Map[String, DogeValue] =
      Map(
        "PrintLn" -> new RawDogeFunc({ args =>
          println(s"${args.map(_.toJvm).mkString}")
          UnitDogeValue
        }),
        "Plus" -> new RawDogeFunc({ args =>
          DogeInt(args.map(_.toJvm).map(_.asInstanceOf[Int]).reduce(_ + _))
        }),
        "IS" -> new RawDogeFunc({ args =>
          args.head
        })
      )
  }


  sealed trait DogeValue {
    def toJvm: AnyRef
  }
  sealed trait DogeLambda extends DogeValue {
    def apply(state: DogeState, args: Seq[DogeValue]): DogeValue
  }
  case class DogeInt(x: Int) extends DogeValue {
    def toJvm = Integer.valueOf(x)
  }

  object UnitDogeValue extends DogeValue {
    override def toJvm = null
  }
  class RawDogeFunc(f: Seq[DogeValue] => DogeValue) extends DogeLambda {
    override def apply(state: DogeState, args: Seq[DogeValue]): DogeValue = f(args)
    override def toJvm = f
  }

  case class CurriedDogeLambda(arg: String, next: DogeLambda) extends DogeLambda {
    override def apply(state: DogeState, args: Seq[DogeValue]): DogeValue =
      args match {
        case head +: tail =>
          val applied = state.put(arg, head)
          next.apply(applied, tail)
        case Seq() => CapturedDogeLambda(state, this)
      }
    override def toJvm = this
  }

  case class CapturedDogeLambda(captured: DogeState, underlying: DogeLambda) extends DogeLambda {
    override def apply(state: DogeState, args: Seq[DogeValue]): DogeValue =
      underlying(captured join state, args)
    override def toJvm = this
  }

  case class Thunk(expr: DogeAst) extends DogeLambda {
    override def apply(state: DogeState, args: Seq[DogeValue]): DogeValue = {
      val(next, result) = interpretNext(expr, state)
      result
    }
    override def toJvm = this
  }

  object DynamicDogeFunc {
    def apply(argNames: Seq[String], definition: DogeAst): DogeLambda = {
      argNames.foldRight[DogeLambda](Thunk(definition)) { case (prev, arg) => CurriedDogeLambda(prev, arg) }
    }
  }

}
