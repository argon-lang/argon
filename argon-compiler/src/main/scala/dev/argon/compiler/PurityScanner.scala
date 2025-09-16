package dev.argon.compiler

import cats.*
import cats.data.State
import cats.implicits.given
import dev.argon.ast.IdentifierExpr
import dev.argon.expr.{BinaryBuiltin, ExprContext, NullaryBuiltin, UnaryBuiltin}
import dev.argon.util.{*, given}

object PurityScanner {
  def apply(context: Context)(ec: context.ArgonExprContext)(expr: ec.Expr): Boolean =
    import ec.*

    trait PureScanner extends TreeScanner[Option] {
      def exprScanner: Scanner[Expr]
    }

    val scanner = new PureScanner {
      import StandardScanners.given

      private val pure: Option[Unit] = Some(())
      private val impure: Option[Unit] = None
      private def fromBool(b: Boolean): Option[Unit] = if b then pure else impure
      private def fromEffectInfo(ei: context.DefaultExprContext.EffectInfo): Option[Unit] =
        ei match {
          case context.DefaultExprContext.EffectInfo.Pure => pure
          case context.DefaultExprContext.EffectInfo.Effectful => impure
        }

      override given exprScanner: Scanner[Expr]:
        override def scan(a: Expr): Option[Unit] =
          a match {
            case Expr.FunctionCall(f, args) =>
              fromEffectInfo(f.effects).flatMap { _ =>
                args.foldM(()) { (_, b) => scan(b) }
              }

            case Expr.Lambda(_, _, _) => pure

            case Expr.Variable(v) => fromBool(!v.isMutable)

            case Expr.VariableStore(v, _) => impure

            case _ => autoScanner[Expr].scan(a)
          }
      end exprScanner

      private given Scanner[Pattern] = autoScanner
      private given Scanner[Builtin] = autoScanner
      private given Scanner[LocalVar] = autoScanner
      private given Scanner[Var] = autoScanner
      given Scanner[Expr.RecordType] = autoScanner
      given Scanner[Expr.EnumType] = autoScanner
      given Scanner[RecordFieldLiteral] = autoScanner
      private given Scanner[RecordFieldPattern] = autoScanner
      private given Scanner[MatchCase] = autoScanner

      private given Scanner[ExpressionOwner] = IgnoreScanner[ExpressionOwner]
      private given Scanner[Function] = IgnoreScanner[Function]
      private given Scanner[Record] = IgnoreScanner[Record]
      private given Scanner[RecordField] = IgnoreScanner[RecordField]
      private given Scanner[Enum] = IgnoreScanner[Enum]
      private given Scanner[EnumVariant] = IgnoreScanner[EnumVariant]
      private given Scanner[Trait] = IgnoreScanner[Trait]
      private given Scanner[NullaryBuiltin] = IgnoreScanner[NullaryBuiltin]
      private given Scanner[UnaryBuiltin] = IgnoreScanner[UnaryBuiltin]
      private given Scanner[BinaryBuiltin] = IgnoreScanner[BinaryBuiltin]
      private given Scanner[Hole] = IgnoreScanner[Hole]

      private given Scanner[UniqueIdentifier] = IgnoreScanner[UniqueIdentifier]
      private given Scanner[IdentifierExpr] = IgnoreScanner[IdentifierExpr]
      private given Scanner[Boolean] = IgnoreScanner[Boolean]
      private given Scanner[BigInt] = IgnoreScanner[BigInt]
      private given Scanner[Int] = IgnoreScanner[Int]
      private given Scanner[String] = IgnoreScanner[String]
    }

    scanner.exprScanner.scan(expr).isDefined
  end apply

}
