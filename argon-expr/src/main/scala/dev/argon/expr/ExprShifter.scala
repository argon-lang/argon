package dev.argon.expr

import cats.*
import dev.argon.ast.IdentifierExpr
import dev.argon.util.{TreeShifter, UniqueIdentifier}

trait ExprShifter[F[_]: Monad] extends TreeShifter[F] {

  val ec1: ExprContext
  val ec2: ExprContext {
    type Function >: ec1.Function
    type Record >: ec1.Record
    type RecordField >: ec1.RecordField
    type Enum >: ec1.Enum
    type Trait >: ec1.Trait
    type EnumVariant >: ec1.EnumVariant
    type Method >: ec1.Method
    type Instance >: ec1.Instance
  }
  
  import StandardShifters.given


  protected def exprShifter: Shifter[ec1.Expr, ec2.Expr] = autoShifter
  protected def patternShifter: Shifter[ec1.Pattern, ec2.Pattern] = autoShifter
  protected def shiftHole(hole: ec1.Hole): F[ec2.Expr]
  protected def varShifter: Shifter[ec1.Var, ec2.Var] = autoShifter
  protected def localVarShifter: Shifter[ec1.LocalVar, ec2.LocalVar] = autoShifter
  
  
  private given Shifter[ec1.Expr, ec2.Expr] = exprShifter
  private given Shifter[ec1.Pattern, ec2.Pattern] = patternShifter
  private given Shifter[ec1.Expr.Hole, ec2.Expr]:
    override def shift(a: ec1.Expr.Hole): F[ec2.Expr] =
      shiftHole(a.hole)
  end given

  private given Shifter[ec1.Builtin, ec2.Builtin] = autoShifter

  private given Shifter[ec1.Expr.RecordType, ec2.Expr.RecordType] = autoShifter
  private given Shifter[ec1.Expr.TraitType, ec2.Expr.TraitType] = autoShifter
  private given Shifter[ec1.Expr.InstanceSingletonType, ec2.Expr.InstanceSingletonType] = autoShifter
  private given Shifter[ec1.RecordFieldLiteral, ec2.RecordFieldLiteral] = autoShifter
  private given Shifter[ec1.Expr.EnumType, ec2.Expr.EnumType] = autoShifter
  private given Shifter[ec1.RecordFieldPattern, ec2.RecordFieldPattern] = autoShifter
  private given Shifter[ec1.MatchCase, ec2.MatchCase] = autoShifter

  private given Shifter[ec1.MethodInstanceType, ec2.MethodInstanceType]:
    override def shift(a: ec1.MethodInstanceType): F[ec2.MethodInstanceType] =
      a match {
        case a: ec1.Expr.TraitType =>
          Functor[F].widen(summon[Shifter[ec1.Expr.TraitType, ec2.Expr.TraitType]].shift(a))
        case a: ec1.Expr.InstanceSingletonType =>
          Functor[F].widen(summon[Shifter[ec1.Expr.InstanceSingletonType, ec2.Expr.InstanceSingletonType]].shift(a))
      }
  end given


  private given Shifter[ec1.Var, ec2.Var] = varShifter
  private given Shifter[ec1.LocalVar, ec2.LocalVar] = localVarShifter
  private given Shifter[ec1.ExpressionOwner, ec2.ExpressionOwner] = autoShifter
  protected final given effectShifter: Shifter[ec1.EffectInfo, ec2.EffectInfo] = autoShifter

  private given Shifter[ec1.Function, ec2.Function] = identityShifter
  private given Shifter[ec1.Record, ec2.Record] = identityShifter
  private given Shifter[ec1.RecordField, ec2.RecordField] = identityShifter
  private given Shifter[ec1.Enum, ec2.Enum] = identityShifter
  private given Shifter[ec1.Trait, ec2.Trait] = identityShifter
  private given Shifter[ec1.EnumVariant, ec2.EnumVariant] = identityShifter
  private given Shifter[ec1.Method, ec2.Method] = identityShifter
  private given Shifter[ec1.Instance, ec2.Instance] = identityShifter

  private given Shifter[NullaryBuiltin, NullaryBuiltin] = identityShifter
  private given Shifter[UnaryBuiltin, UnaryBuiltin] = identityShifter
  private given Shifter[BinaryBuiltin, BinaryBuiltin] = identityShifter
  private given Shifter[IdentifierExpr, IdentifierExpr] = identityShifter
  private given Shifter[UniqueIdentifier, UniqueIdentifier] = identityShifter
  private given [E <: ErasureMode] => Shifter[E, E] = identityShifter
}
