package dev.argon.expr

trait ContextShifter[F[_]] extends ExprShifter[F] {

  final def shiftExpr(a: ec1.Expr): F[ec2.Expr] =
    exprShifter.shift(a)
    
  final def shiftVar(v: ec1.Var): F[ec2.Var] =
    varShifter.shift(v)
    
  final def shiftEffectInfo(eff: ec1.EffectInfo): F[ec2.EffectInfo] =
    effectShifter.shift(eff)
}
