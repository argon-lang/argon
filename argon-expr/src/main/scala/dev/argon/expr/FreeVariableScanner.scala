package dev.argon.expr

import cats.*
import cats.implicits.given
import dev.argon.ast.IdentifierExpr
import dev.argon.util.{*, given}
import cats.data.State

object FreeVariableScanner {
  def apply(ec: ExprContext { type Hole = Nothing })(expr: ec.Expr): Seq[ec.Var] =
    import ec.*

    final case class ScanState(seenVars: Set[ec.Var], freeVars: Seq[ec.Var])

    final class VarScanner extends ExprScanner[[A] =>> State[ScanState, A]] {
      import StandardScanners.given
      override val exprContext: ec.type = ec

      override given exprScanner: Scanner[Expr]:
        override def scan(a: Expr): State[ScanState, Unit] =
          a match {
            case Expr.BindVariable(v, _) =>
              State.modify[ScanState](s => s.copy(seenVars = s.seenVars + v))
                *> VarScanner.super.exprScanner.scan(a)

            case Expr.Lambda(v, _, _) =>
              State.modify[ScanState](s => s.copy(seenVars = s.seenVars + v))
                *> VarScanner.super.exprScanner.scan(a)

            case Expr.FunctionType(v, _) =>
              State.modify[ScanState](s => s.copy(seenVars = s.seenVars + v))
                *> VarScanner.super.exprScanner.scan(a)

            case Expr.IfElse(whenTrueWitness, whenFalseWitness, _, _, _) =>
              State.modify[ScanState](s => s.copy(seenVars = s.seenVars ++ whenTrueWitness.toSet[ec.Var] ++ whenFalseWitness.toSet[ec.Var]))
                *> VarScanner.super.exprScanner.scan(a)

            case Expr.Variable(v) =>
              VarScanner.super.exprScanner.scan(v.varType) *>
                State.modify[ScanState] { s =>
                  if s.seenVars.contains(v) then
                    s
                  else
                    s.copy(seenVars = s.seenVars + v, freeVars = s.freeVars :+ v)
                }

            case Expr.VariableStore(v, value) =>
              VarScanner.super.exprScanner.scan(v.varType) *>
                State.modify[ScanState] { s =>
                  if s.seenVars.contains(v) then
                    s
                  else
                    s.copy(seenVars = s.seenVars + v, freeVars = s.freeVars :+ v)
                } *>
                VarScanner.super.exprScanner.scan(value)

            case _ => VarScanner.super.exprScanner.scan(a)
          }
      end exprScanner


      override protected def holeScanner: Scanner[Nothing] = summon
    }

    val scanner = VarScanner()

    val res = scanner.exprScanner.scan(expr).runF.value(ScanState(Set.empty, Seq.empty)).value._1

    res.freeVars
  end apply
}
