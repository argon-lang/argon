package dev.argon.expr

import cats.Monad
import dev.argon.ast.IdentifierExpr
import dev.argon.util.{TreeShifter, UniqueIdentifier}

import scala.reflect.TypeTest

trait ExprContext {

  sealed trait Var
  final case class LocalVar(
    id: UniqueIdentifier,
    varType: WExpr,
    name: Option[IdentifierExpr],
    isMutable: Boolean,
    isErased: Boolean,
    isGiven: Boolean,
  ) extends Var

  type Function
  given functionCanEqual: CanEqual[Function, Function]

  type Hole
  given holeCanEqual: CanEqual[Hole, Hole]


  enum Builtin {
    case Nullary(builtin: NullaryBuiltin)
    case Unary(builtin: UnaryBuiltin, a: WExpr)
    case Binary(builtin: BinaryBuiltin, a: WExpr, b: WExpr)

    case EqualTo(t: WExpr, a: WExpr, b: WExpr)
  }

  enum Expr {
    case BindVariable(v: LocalVar, value: WExpr)
    case BoolLiteral(b: Boolean)
    case Builtin(b: ExprContext.this.Builtin)
    case FunctionCall(f: Function, args: Seq[WExpr])
    case FunctionObjectCall(f: WExpr, a: WExpr)
    case FunctionType(a: WExpr, r: WExpr)
    case IfElse(
      whenTrueWitness: Option[LocalVar],
      whenFalseWitness: Option[LocalVar],
      condition: WExpr,
      trueBody: WExpr,
      falseBody: WExpr,
    )
    case IntLiteral(i: BigInt)
    case Sequence(stmts: Seq[WExpr], result: WExpr)
    case StoreVariable(v: Var, value: WExpr)
    case Tuple(items: Seq[WExpr])
    case TupleElement(index: Int, tuple: WExpr)
    case Variable(v: Var)
  }

  enum WExpr {
    case Normal(e: Expr)
    case Hole(h: ExprContext.this.Hole)
    case Error()
  }

  final class Model private(mapping: Map[Hole, WExpr]) {
    def resolveHole(hole: Hole): Option[WExpr] =
      mapping.get(hole)

    private[expr] def addMapping(hole: Hole, expr: WExpr): Model =
      Model(mapping.updated(hole, expr))
  }

  object Model {
    def empty: Model = Model(Map.empty)
  }

}
