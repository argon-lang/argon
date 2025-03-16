package dev.argon.expr

import cats.Monad
import dev.argon.ast.IdentifierExpr
import dev.argon.util.{TreeShifter, UniqueIdentifier}

import scala.reflect.TypeTest
import dev.argon.util.SourceLocation

import java.util.Objects
import scala.compiletime.{asMatchable, deferred}

trait ExprContext {

  sealed trait Var {
    def name: Option[IdentifierExpr]
    def varType: Expr
    def isErased: Boolean
    def isMutable: Boolean
    def isProof: Boolean
  }

  final case class LocalVar(
    id: UniqueIdentifier,
    varType: Expr,
    name: Option[IdentifierExpr],
    isMutable: Boolean,
    isErased: Boolean,
    isProof: Boolean,
  ) extends Var {
    override def hashCode(): Int = id.hashCode()

    override def equals(obj: Any): Boolean =
      obj.asMatchable match {
        case other: LocalVar => id == other.id
        case _ => false
      }
  }
  
  enum ParameterOwner derives CanEqual {
    case Func(f: Function)
    case Rec(r: Record)
  }
  
  final case class ParameterVar(
    owner: ParameterOwner,
    parameterIndex: Int,
    varType: Expr,
    name: Option[IdentifierExpr],
    isErased: Boolean,
    isProof: Boolean,
  ) extends Var {
    override def isMutable: Boolean = false

    override def hashCode(): Int =
      Objects.hash(owner, parameterIndex)

    override def equals(obj: Any): Boolean =
      obj.asMatchable match {
        case other: ParameterVar =>
          owner == other.owner && parameterIndex == other.parameterIndex

        case _ => false
      }
  }

  type Function <: Matchable
  given functionCanEqual: CanEqual[Function, Function] = deferred

  type Record <: Matchable
  given recordCanEqual: CanEqual[Record, Record] = deferred

  type RecordField
  given recordFieldCanEqual: CanEqual[RecordField, RecordField] = deferred
  def getRecordFieldName(f: RecordField): IdentifierExpr

  type Hole
  given holeCanEqual: CanEqual[Hole, Hole] = deferred


  enum Builtin {
    case Nullary(builtin: NullaryBuiltin)
    case Unary(builtin: UnaryBuiltin, a: Expr)
    case Binary(builtin: BinaryBuiltin, a: Expr, b: Expr)

    case EqualTo(t: Expr, a: Expr, b: Expr)
    case EqualToRefl(t: Expr, a: Expr)
  }

  enum Expr derives CanEqual {
    case Error()
    case ErasedValue()

    case Hole(hole: ExprContext.this.Hole)
    
    case AnyType()
    case BindVariable(v: LocalVar, value: Expr)
    case BoolLiteral(b: Boolean)
    case Builtin(b: ExprContext.this.Builtin)
    case Finally(action: Expr, ensuring: Expr)
    case FunctionCall(f: Function, args: Seq[Expr])
    case FunctionObjectCall(f: Expr, a: Expr)
    case FunctionType(a: LocalVar, r: Expr)
    case IfElse(
      whenTrueWitness: Option[LocalVar],
      whenFalseWitness: Option[LocalVar],
      condition: Expr,
      trueBody: Expr,
      falseBody: Expr,
    )
    case IntLiteral(i: BigInt)
    case Lambda(v: LocalVar, returnType: Expr, body: Expr)
    case RecordType(record: Record, args: Seq[Expr])
    case RecordFieldLoad(record: Expr.RecordType, field: RecordField, recordValue: Expr)
    case RecordFieldStore(record: Expr.RecordType, field: RecordField, recordValue: Expr, fieldValue: Expr)
    case RecordLiteral(record: Expr.RecordType, fields: Seq[RecordFieldLiteral])
    case Sequence(stmts: Seq[Expr], result: Expr)
    case StringLiteral(s: String)
    case Tuple(items: Seq[Expr])
    case TupleElement(index: Int, tuple: Expr)
    case TypeN(n: Expr)
    case TypeBigN(n: BigInt)
    case Variable(v: Var)
    case VariableStore(v: Var, value: Expr)
  }

  final case class RecordFieldLiteral(
    field: RecordField,
    value: Expr,
  )

  final case class AnnotatedExpr(location: SourceLocation, e: Expr, t: Expr)

  enum EffectInfo derives CanEqual {
    case Pure, Effectful
  }

  final class Model private(mapping: Map[Hole, Expr]) {
    def resolveHole(hole: Hole): Option[Expr] =
      mapping.get(hole)

    private[expr] def addMapping(hole: Hole, expr: Expr): Model =
      Model(mapping.updated(hole, expr))

    override def toString(): String = s"Model: $mapping"
  }

  object Model {
    def empty: Model = Model(Map.empty)
  }

}
