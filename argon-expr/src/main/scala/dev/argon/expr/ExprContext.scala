package dev.argon.expr

import cats.Monad
import dev.argon.ast.IdentifierExpr
import dev.argon.util.{TreeShifter, UniqueIdentifier}

import scala.reflect.TypeTest

trait ExprContext {

  sealed trait Var {
    def name: Option[IdentifierExpr]
    def varType: Expr
  }

  final case class LocalVar(
    id: UniqueIdentifier,
    varType: Expr,
    name: Option[IdentifierExpr],
    isMutable: Boolean,
    isErased: Boolean,
    isProof: Boolean,
  ) extends Var
  
  type ParameterOwner = Function
  
  final case class ParameterVar(
    owner: ParameterOwner,
    parameterIndex: Int,
    tupleIndex: Option[Int],
    varType: Expr,
    name: Option[IdentifierExpr],
    isErased: Boolean,
    isProof: Boolean,
  ) extends Var

  type Function
  given functionCanEqual: CanEqual[Function, Function]

  type Hole
  given holeCanEqual: CanEqual[Hole, Hole]


  enum Builtin {
    case Nullary(builtin: NullaryBuiltin)
    case Unary(builtin: UnaryBuiltin, a: Expr)
    case Binary(builtin: BinaryBuiltin, a: Expr, b: Expr)

    case EqualTo(t: Expr, a: Expr, b: Expr)
  }

  enum Expr {
    case Error()
    case Hole(hole: ExprContext.this.Hole)
    
    
    case BindVariable(v: LocalVar, value: Expr)
    case BoolLiteral(b: Boolean)
    case Builtin(b: ExprContext.this.Builtin)
    case FunctionCall(f: Function, args: Seq[Expr])
    case FunctionObjectCall(f: Expr, a: Expr)
    case FunctionType(a: Expr, r: Expr)
    case IfElse(
      whenTrueWitness: Option[LocalVar],
      whenFalseWitness: Option[LocalVar],
      condition: Expr,
      trueBody: Expr,
      falseBody: Expr,
    )
    case IntLiteral(i: BigInt)
    case Sequence(stmts: Seq[Expr], result: Expr)
    case StringLiteral(s: String)
    case StoreVariable(v: Var, value: Expr)
    case Tuple(items: Seq[Expr])
    case TupleElement(index: Int, tuple: Expr)
    case TypeN(n: Expr)
    case TypeBigN(n: BigInt)
    case Variable(v: Var)
  }

  final class Model private(mapping: Map[Hole, Expr]) {
    def resolveHole(hole: Hole): Option[Expr] =
      mapping.get(hole)

    private[expr] def addMapping(hole: Hole, expr: Expr): Model =
      Model(mapping.updated(hole, expr))
  }

  object Model {
    def empty: Model = Model(Map.empty)
  }

}
