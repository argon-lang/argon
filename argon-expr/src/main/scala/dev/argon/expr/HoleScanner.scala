package dev.argon.expr

import cats.*
import dev.argon.ast.IdentifierExpr
import dev.argon.util.{*, given}

trait HoleScanner extends TreeScanner[[A] =>> Either[Unit, A]] {
  val exprContext: ExprContext
  import exprContext.*

  val searchTarget: Hole

  import StandardScanners.given

  private given Scanner[Hole] with
    override def scan(a: Hole): Either[Unit, Unit] =
      if a == searchTarget then
        Left(())
      else
        Right(())
  end given

  given exprScanner: Scanner[Expr] = autoScanner
  private given Scanner[Builtin] = autoScanner
  private given Scanner[LocalVar] = autoScanner
  private given Scanner[Var] = autoScanner
  given Scanner[Expr.RecordType] = autoScanner
  given Scanner[RecordFieldLiteral] = autoScanner

  private given Scanner[ParameterOwner] = IgnoreScanner[ParameterOwner]
  private given Scanner[Function] = IgnoreScanner[Function]
  private given Scanner[Record] = IgnoreScanner[Record]
  private given Scanner[RecordField] = IgnoreScanner[RecordField]
  private given Scanner[NullaryBuiltin] = IgnoreScanner[NullaryBuiltin]
  private given Scanner[UnaryBuiltin] = IgnoreScanner[UnaryBuiltin]
  private given Scanner[BinaryBuiltin] = IgnoreScanner[BinaryBuiltin]

  private given Scanner[UniqueIdentifier] = IgnoreScanner[UniqueIdentifier]
  private given Scanner[IdentifierExpr] = IgnoreScanner[IdentifierExpr]
  private given Scanner[Boolean] = IgnoreScanner[Boolean]
  private given Scanner[BigInt] = IgnoreScanner[BigInt]
  private given Scanner[Int] = IgnoreScanner[Int]
  private given Scanner[String] = IgnoreScanner[String]




}
