package dev.argon.expr

import cats.Monad
import dev.argon.ast.IdentifierExpr
import dev.argon.util.{TreeShifter, UniqueIdentifier}

import scala.reflect.TypeTest
import dev.argon.util.SourceLocation

import java.util.Objects
import scala.compiletime.{asMatchable, deferred}

trait ExprContext extends ConditionalVars {

  sealed trait Var {
    def name: Option[IdentifierExpr]
    def varType: Expr
    def erasureMode: ErasureMode.Declared
    def isMutable: Boolean
    def isWitness: Boolean
  }

  final case class LocalVar(
    id: UniqueIdentifier,
    varType: Expr,
    name: Option[IdentifierExpr],
    isMutable: Boolean,
    erasureMode: ErasureMode.DeclaredNonToken,
    isWitness: Boolean,
  ) extends Var {
    override def hashCode(): Int = id.hashCode()

    override def equals(obj: Any): Boolean =
      obj.asMatchable match {
        case other: LocalVar => id == other.id
        case _ => false
      }
  }
  
  enum ExpressionOwner derives CanEqual {
    case Func(f: Function)
    case Rec(r: Record)
    case Enum(e: ExprContext.this.Enum)
    case Trait(t: ExprContext.this.Trait)
    case EnumVariant(v: ExprContext.this.EnumVariant)
    case Method(m: ExprContext.this.Method)
    case Instance(i: ExprContext.this.Instance)
  }
  
  final case class ParameterVar(
    owner: ExpressionOwner,
    parameterIndex: Int,
    varType: Expr,
    name: Option[IdentifierExpr],
    erasureMode: ErasureMode.Declared,
    isWitness: Boolean,
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

  final case class InstanceParameterVar(
    owner: ExpressionOwner,
    varType: Expr,
    name: Option[IdentifierExpr],
  ) extends Var {
    override def erasureMode: ErasureMode.Concrete.type = ErasureMode.Concrete
    override def isMutable: Boolean = false
    override def isWitness: Boolean = false
  }

  type Function <: Matchable
  given functionCanEqual: CanEqual[Function, Function] = deferred

  type Record <: Matchable
  given recordCanEqual: CanEqual[Record, Record] = deferred

  type RecordField
  given recordFieldCanEqual: CanEqual[RecordField, RecordField] = deferred
  def getRecordFieldName(f: RecordField): IdentifierExpr

  type Enum
  given enumCanEqual: CanEqual[Enum, Enum] = deferred

  type EnumVariant
  given enumVariantCanEqual: CanEqual[EnumVariant, EnumVariant] = deferred

  type Trait
  given traitCanEqual: CanEqual[Trait, Trait] = deferred

  type Method
  given methodCanEqual: CanEqual[Method, Method] = deferred

  type Instance
  given instanceCanEqual: CanEqual[Instance, Instance] = deferred

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

    case And(a: Expr, b: Expr)
    case AnyType()
    case BindVariable(v: LocalVar, value: Expr)
    case BoolLiteral(b: Boolean)
    case Box(t: Expr, value: Expr) // Convert values of T to boxed T    
    case Boxed(t: Expr) // Use an erased type in a non-erased context.
    case Builtin(b: ExprContext.this.Builtin)
    case EnumVariantLiteral(enumType: EnumType, v: ExprContext.this.EnumVariant, args: Seq[Expr], fields: Seq[RecordFieldLiteral])
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
    case InstanceMethodCall(m: Method, instanceType: MethodInstanceType, obj: Expr, args: Seq[Expr])
    case InstanceSingletonType(i: Instance, args: Seq[Expr])
    case IntLiteral(i: BigInt)
    case Is(value: Expr, pattern: Pattern)
    case Lambda(v: LocalVar, returnType: Expr, body: Expr)
    case Match(value: Expr, cases: Seq[MatchCase])
    case NewInstance(i: Instance, args: Seq[Expr])
    case Or(a: Expr, b: Expr)
    case RecordType(record: Record, args: Seq[Expr])
    case EnumType(e: Enum, args: Seq[Expr])
    case RecordFieldLoad(record: Expr.RecordType, field: RecordField, recordValue: Expr)
    case RecordFieldStore(record: Expr.RecordType, field: RecordField, recordValue: Expr, fieldValue: Expr)
    case RecordLiteral(record: Expr.RecordType, fields: Seq[RecordFieldLiteral])
    case Sequence(stmts: Seq[Expr], result: Expr)
    case StringLiteral(s: String)
    case TraitType(t: Trait, args: Seq[Expr])
    case Tuple(items: Seq[Expr])
    case TupleElement(index: Int, tuple: Expr)
    case TypeN(n: Expr)
    case TypeBigN(n: BigInt)
    case Unbox(t: Expr, value: Expr) // Convert value of boxed T to T
    case Variable(v: Var)
    case VariableStore(v: Var, value: Expr)
  }

  enum Pattern derives CanEqual {
    case Discard(t: Expr)
    case Tuple(elements: Seq[Pattern])
    case Binding(v: LocalVar, pattern: Pattern)
    case EnumVariant(enumType: Expr.EnumType, variant: ExprContext.this.EnumVariant, args: Seq[Pattern], fields: Seq[RecordFieldPattern])
    case String(s: java.lang.String)
    case Int(i: BigInt)
    case Bool(b: Boolean)
  }

  final case class MatchCase(
    pattern: Pattern,
    body: Expr,
  )

  final case class RecordFieldPattern(
    field: RecordField,
    pattern: Pattern,
  )

  final case class RecordFieldLiteral(
    field: RecordField,
    value: Expr,
  )

  type MethodInstanceType = Expr.TraitType | Expr.InstanceSingletonType

  final case class AnnotatedExpr(location: SourceLocation, e: Expr, t: Expr)

  enum EffectInfo derives CanEqual {
    case Pure, Effectful
  }

  final class Model private(val mapping: Map[Hole, Expr]) {
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
