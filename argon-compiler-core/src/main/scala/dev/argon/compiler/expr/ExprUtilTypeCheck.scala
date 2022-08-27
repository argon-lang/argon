package dev.argon.compiler.expr

import dev.argon.compiler.*
import dev.argon.util.{*, given}
import zio.*

trait ExprUtilTypeCheck
  extends ExprUtilWithHolesBase
    with ExprUtilImplicitResolver
{
  import exprContext.{ArExpr, ExprConstructor, Variable, WrapExpr}


  // Ensures that a <: b
  def checkSubType(env: Env, a: WrapExpr, b: WrapExpr, location: SourceLocation): Comp[Env] =
    isSubType(env, a, b).flatMap {
      case Some(env) => ZIO.succeed(env)
      case None =>
        ZIO.logTrace(s"checkSubType failed a=$a, b=$b") *>
          ZIO.fail(DiagnosticError.TypeError(DiagnosticSource.Location(location)))
    }

  def isSubType(env: Env, a: WrapExpr, b: WrapExpr): Comp[Option[Env]] =
    if isSubTypeQuick(a, b) then
      ZIO.succeed(Some(env))
    else
      val prop = WrapExpr.OfExpr(ArExpr(ExprConstructor.SubtypeWitnessType, (a, b)))
      tryResolveImplicit(env, prop).map { _.map { case (env, _) => env } }
    end if


  // Fast paths are needed to avoid circular loading.
  // Type checker needs to load (<): (Nat, Nat) -> Bool which requires type checking.
  // This implements enough of the type checker to load this function.
  private def isSubTypeQuick(a: WrapExpr, b: WrapExpr): Boolean =
    a.equals(b) || ((a, b) match {
      case (WrapExpr.OfExpr(a), WrapExpr.OfExpr(b)) =>
        (a.constructor, b.constructor) match {
          case (ExprConstructor.TypeN, ExprConstructor.AnyType) => true
          case (ExprConstructor.TypeN, ExprConstructor.OmegaTypeN(_)) => true
          case (ExprConstructor.OmegaTypeN(n), ExprConstructor.OmegaTypeN(m)) => n <= m
          case _ => false
        }

      case _ => false
    })

  def isSameType(env: Env, a: WrapExpr, b: WrapExpr): Comp[Option[Env]] =
    isSubType(env, a, b).flatMap {
      case Some(env) => isSubType(env, b, a)
      case None => ZIO.none
    }
}
