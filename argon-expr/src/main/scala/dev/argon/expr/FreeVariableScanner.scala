package dev.argon.expr

import cats.*
import cats.implicits.given
import dev.argon.ast.IdentifierExpr
import dev.argon.util.{*, given}
import cats.data.State

object FreeVariableScanner {
  def apply(ec: ExprContext { type Hole = Nothing })(expr: ec.Expr): Set[ec.Var] =
    import ec.*

    final case class ScanState(seenVars: Set[ec.Var], declVars: Set[ec.Var])

    trait VarScanner extends TreeScanner[[A] =>> State[ScanState, A]] {
      def exprScanner: Scanner[Expr]
    }

    val scanner = new VarScanner {

      import StandardScanners.given

      override given exprScanner: Scanner[Expr]:
        override def scan(a: Expr): State[ScanState, Unit] =
          (a match {
            case Expr.BindVariable(v, _) =>
              State.modify[ScanState](s => s.copy(declVars = s.declVars + v))

            case Expr.Variable(v) =>
              State.modify[ScanState](s => s.copy(seenVars = s.seenVars + v))

            case Expr.StoreVariable(v, _) =>
              State.modify[ScanState](s => s.copy(seenVars = s.seenVars + v))

            case _ => State.pure[ScanState, Unit](())
          }) *> autoScanner[Expr].scan(a)
      end exprScanner
      
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

    val res = scanner.exprScanner.scan(expr).runF.value(ScanState(Set.empty, Set.empty)).value._1

    res.seenVars -- res.declVars
  end apply
}
