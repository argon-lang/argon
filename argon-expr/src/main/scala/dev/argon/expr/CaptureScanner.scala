package dev.argon.expr

import cats.*
import cats.implicits.given
import dev.argon.util.*
import cats.data.State

object CaptureScanner {

  def apply(ec: ExprContext { type Hole = Nothing })(expr: ec.Expr): Set[ec.Var] =
    import ec.*

    final case class ScanState(capturedVars: Set[ec.Var])

    final class VarScanner extends ExprScanner[[A] =>> State[ScanState, A]] {
      import StandardScanners.given
      
      override val exprContext: ec.type = ec

      override def exprScanner: Scanner[Expr] = new Scanner[Expr] {
        override def scan(a: Expr): State[ScanState, Unit] =
          (a match {
            case Expr.Lambda(_, _, _) =>
              val captured = FreeVariableScanner.apply(ec)(a)
              State.modify[ScanState](s => s.copy(capturedVars = s.capturedVars ++ captured))

            case _ => State.pure[ScanState, Unit](())
          }) *> VarScanner.super.exprScanner.scan(a)
      }

      override protected def holeScanner: Scanner[Nothing] = summon
    }
    
    val scanner = VarScanner()

    scanner.exprScanner
      .scan(expr)
      .runS(ScanState(Set.empty))
      .value
      .capturedVars
  end apply
}
