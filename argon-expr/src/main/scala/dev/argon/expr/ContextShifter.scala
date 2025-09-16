package dev.argon.expr

import dev.argon.util.{TreeShifter, UniqueIdentifier}
import cats.*
import cats.implicits.given
import zio.ZIO
import zio.interop.catz.core.given
import dev.argon.ast.IdentifierExpr

trait ContextShifter[F[_]: Monad] {

  val ec1: ExprContext
  val ec2: ExprContext {
    type Function >: ec1.Function
    type Record >: ec1.Record
    type RecordField >: ec1.RecordField
    type Enum >: ec1.Enum
    type Trait >: ec1.Trait
    type EnumVariant >: ec1.EnumVariant
    type Method >: ec1.Method
  }


  private object ShiftHelper extends TreeShifter[F] {
    import StandardShifters.given

    given exprShifter: Shifter[ec1.Expr, ec2.Expr] = autoShifter
    given Shifter[ec1.Pattern, ec2.Pattern] = autoShifter
    
    given Shifter[ec1.Builtin, ec2.Builtin] = autoShifter
    given Shifter[ec1.Expr.Hole, ec2.Expr]:
      override def shift(a: ec1.Expr.Hole): F[ec2.Expr] =
        shiftHole(a.hole)
    end given
    given Shifter[ec1.Expr.RecordType, ec2.Expr.RecordType] = autoShifter
    given Shifter[ec1.RecordFieldLiteral, ec2.RecordFieldLiteral] = autoShifter
    given Shifter[ec1.Expr.EnumType, ec2.Expr.EnumType] = autoShifter
    given Shifter[ec1.RecordFieldPattern, ec2.RecordFieldPattern] = autoShifter
    given Shifter[ec1.MatchCase, ec2.MatchCase] = autoShifter


    given varShifter: Shifter[ec1.Var, ec2.Var] = autoShifter
    given Shifter[ec1.LocalVar, ec2.LocalVar] = autoShifter
    given Shifter[ec1.ExpressionOwner, ec2.ExpressionOwner] = autoShifter
    
    given effectShifter: Shifter[ec1.EffectInfo, ec2.EffectInfo] = autoShifter

    given Shifter[ec1.Function, ec2.Function] = identityShifter
    given Shifter[ec1.Record, ec2.Record] = identityShifter
    given Shifter[ec1.RecordField, ec2.RecordField] = identityShifter
    given Shifter[ec1.Enum, ec2.Enum] = identityShifter
    given Shifter[ec1.Trait, ec2.Trait] = identityShifter
    given Shifter[ec1.EnumVariant, ec2.EnumVariant] = identityShifter
    given Shifter[ec1.Method, ec2.Method] = identityShifter
    
    given Shifter[NullaryBuiltin, NullaryBuiltin] = identityShifter
    given Shifter[UnaryBuiltin, UnaryBuiltin] = identityShifter
    given Shifter[BinaryBuiltin, BinaryBuiltin] = identityShifter

    given Shifter[IdentifierExpr, IdentifierExpr] = identityShifter
    given Shifter[UniqueIdentifier, UniqueIdentifier] = identityShifter

  }

  final def shiftExpr(a: ec1.Expr): F[ec2.Expr] =
    ShiftHelper.exprShifter.shift(a)
    
  final def shiftVar(v: ec1.Var): F[ec2.Var] =
    ShiftHelper.varShifter.shift(v)
    
  final def shiftEffectInfo(eff: ec1.EffectInfo): F[ec2.EffectInfo] =
    ShiftHelper.effectShifter.shift(eff)

  protected def shiftHole(hole: ec1.Hole): F[ec2.Expr]

}
