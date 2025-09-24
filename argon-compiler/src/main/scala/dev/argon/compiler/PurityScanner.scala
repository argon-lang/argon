package dev.argon.compiler

import cats.*
import cats.data.State
import cats.implicits.given
import dev.argon.ast.IdentifierExpr
import dev.argon.expr.{BinaryBuiltin, ExprContext, NullaryBuiltin, UnaryBuiltin, ExprScanner}
import dev.argon.util.{*, given}

object PurityScanner {
  def apply(context: Context)(ec: context.ArgonExprContext)(expr: ec.Expr): Boolean =
    import ec.*

    final class PureScanner extends ExprScanner[Option] {
      import StandardScanners.given
      override val exprContext: ec.type = ec
      
      private val pure: Option[Unit] = Some(())
      private val impure: Option[Unit] = None

      private def fromBool(b: Boolean): Option[Unit] = if b then pure else impure

      private def fromEffectInfo(ei: context.DefaultExprContext.EffectInfo): Option[Unit] =
        ei match {
          case context.DefaultExprContext.EffectInfo.Pure => pure
          case context.DefaultExprContext.EffectInfo.Effectful => impure
        }

      override def exprScanner: Scanner[Expr] = new Scanner[Expr] {
        override def scan(a: Expr): Option[Unit] =
          a match {
            case Expr.FunctionCall(f, _) =>
              fromEffectInfo(f.effects) *>
                PureScanner.super.exprScanner.scan(a)

            case Expr.InstanceMethodCall(m, _, _, _) =>
              fromEffectInfo(m.effects) *>
                PureScanner.super.exprScanner.scan(a)

            case Expr.Lambda(_, _, _) => pure

            case Expr.Variable(v) => fromBool(!v.isMutable)

            case Expr.VariableStore(v, _) => impure

            case _ => PureScanner.super.exprScanner.scan(a)
          }
      }

      override protected def holeScanner: Scanner[Hole] = ignoreScanner
    }

    val scanner = PureScanner()

    scanner.exprScanner.scan(expr).isDefined
  end apply

}
