package dev.argon.expr

import cats.*
import cats.data.State
import cats.implicits.given
import dev.argon.ast.IdentifierExpr
import dev.argon.util.{*, given}

import scala.compiletime.deferred

trait ExprScanner[F[_]: Monad] extends TreeScanner[F] {
  import StandardScanners.given

  val exprContext: ExprContext
  import exprContext.*
  

  protected def exprScanner: Scanner[Expr] = autoScanner
  protected def patternScanner: Scanner[Pattern] = autoScanner
  protected def holeScanner: Scanner[Hole]
  

  private given Scanner[Expr] = exprScanner
  private given Scanner[Pattern] = patternScanner
  private given Scanner[Hole] = holeScanner
  
  private given Scanner[Builtin] = autoScanner
  private given Scanner[LocalVar] = autoScanner
  private given Scanner[Var] = autoScanner

  private given Scanner[Expr.RecordType] = autoScanner
  private given Scanner[Expr.EnumType] = autoScanner
  private given Scanner[Expr.TraitType] = autoScanner

  private given Scanner[MethodInstanceType]:
    override def scan(a: MethodInstanceType): F[Unit] =
      exprScanner.scan(a)
  end given


  private given Scanner[RecordFieldLiteral] = autoScanner
  private given Scanner[RecordFieldPattern] = autoScanner
  private given Scanner[MatchCase] = autoScanner
  private given Scanner[ExpressionOwner] = ignoreScanner

  private given Scanner[Function] = ignoreScanner
  private given Scanner[Record] = ignoreScanner
  private given Scanner[RecordField] = ignoreScanner
  private given Scanner[Enum] = ignoreScanner
  private given Scanner[EnumVariant] = ignoreScanner
  private given Scanner[Trait] = ignoreScanner
  private given Scanner[Method] = ignoreScanner
  private given Scanner[Instance] = ignoreScanner

  private given Scanner[NullaryBuiltin] = ignoreScanner
  private given Scanner[UnaryBuiltin] = ignoreScanner
  private given Scanner[BinaryBuiltin] = ignoreScanner

  private given Scanner[UniqueIdentifier] = ignoreScanner
  private given Scanner[IdentifierExpr] = ignoreScanner
  private given Scanner[Boolean] = ignoreScanner
  private given Scanner[BigInt] = ignoreScanner
  private given Scanner[Int] = ignoreScanner
  private given Scanner[String] = ignoreScanner
  private given [E <: ErasureMode] => Scanner[E] = ignoreScanner
}
