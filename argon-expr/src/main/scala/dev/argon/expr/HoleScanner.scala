package dev.argon.expr

import cats.*
import dev.argon.ast.IdentifierExpr
import dev.argon.util.{*, given}

trait HoleScanner extends TreeScanner[[A] =>> Either[Unit, A]] {
  val exprContext: ExprContext
  import exprContext.*

  val searchTarget: Hole

  import StandardScanners.given

  given wexprScanner: Scanner[WExpr] = autoScanner

  private given Scanner[Hole] with
    override def scan(a: Hole): Either[Unit, Unit] =
      if a == searchTarget then
        Left(())
      else
        Right(())
  end given

  private given Scanner[Expr] = autoScanner
  private given Scanner[Builtin] = autoScanner
  private given Scanner[LocalVar] = autoScanner
  private given Scanner[Var] = autoScanner

  private given Scanner[Function] = IgnoreScanner[Function]
  private given Scanner[NullaryBuiltin] = IgnoreScanner[NullaryBuiltin]
  private given Scanner[UnaryBuiltin] = IgnoreScanner[UnaryBuiltin]
  private given Scanner[BinaryBuiltin] = IgnoreScanner[BinaryBuiltin]

  private given Scanner[UniqueIdentifier] = IgnoreScanner[UniqueIdentifier]
  private given Scanner[IdentifierExpr] = IgnoreScanner[IdentifierExpr]
  private given Scanner[Boolean] = IgnoreScanner[Boolean]
  private given Scanner[BigInt] = IgnoreScanner[BigInt]
  private given Scanner[Int] = IgnoreScanner[Int]




}
