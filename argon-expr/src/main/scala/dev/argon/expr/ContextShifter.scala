package dev.argon.expr

import dev.argon.util.{TreeShifter, UniqueIdentifier}
import cats.*
import cats.implicits.given
import zio.ZIO
import zio.interop.catz.core.given
import dev.argon.ast.IdentifierExpr

trait ContextShifter[F[_]: Monad] extends ExprShifter[F] {

  final def shiftExpr(a: ec1.Expr): F[ec2.Expr] =
    exprShifter.shift(a)
    
  final def shiftVar(v: ec1.Var): F[ec2.Var] =
    varShifter.shift(v)
    
  final def shiftEffectInfo(eff: ec1.EffectInfo): F[ec2.EffectInfo] =
    effectShifter.shift(eff)
}
