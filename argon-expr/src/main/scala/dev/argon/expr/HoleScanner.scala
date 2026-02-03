package dev.argon.expr

import cats.*
import dev.argon.util.*

private[expr] sealed trait HoleScanner extends ExprScanner[[A] =>> Either[Unit, A]] {
  val exprContext: ExprContext
  import exprContext.*

  val searchTarget: Hole

  override protected def holeScanner: Scanner[Hole] = new Scanner[Hole] {
    override def scan(a: Hole): Either[Unit, Unit] =
      if a == searchTarget then
        Left(())
      else
        Right(())
  }

  override def exprScanner: Scanner[exprContext.Expr] = super.exprScanner
}

object HoleScanner {
  def hasHole(ec: ExprContext)(hole: ec.Hole)(expr: ec.Expr): Boolean =
    new HoleScanner {
      override val exprContext: ec.type = ec
      override val searchTarget: exprContext.Hole = hole
    }.exprScanner.scan(expr).isLeft
}
