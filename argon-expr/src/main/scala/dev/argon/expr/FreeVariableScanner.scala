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

    trait VarScanner extends TreeScanner[[A] =>> State[ScanState, A]] {
      def exprScanner: Scanner[Expr]
    }

    val scanner = new VarScanner {

      import StandardScanners.given

      override given exprScanner: Scanner[Expr]:
        override def scan(a: Expr): State[ScanState, Unit] =
          a match {
            case Expr.BindVariable(v, _) =>
              State.modify[ScanState](s => s.copy(seenVars = s.seenVars + v))
                *> exprBaseScanner.scan(a)

            case Expr.Lambda(v, _, _) =>
              State.modify[ScanState](s => s.copy(seenVars = s.seenVars + v))
                *> exprBaseScanner.scan(a)

            case Expr.FunctionType(v, _) =>
              State.modify[ScanState](s => s.copy(seenVars = s.seenVars + v))
                *> exprBaseScanner.scan(a)

            case Expr.IfElse(whenTrueWitness, whenFalseWitness, _, _, _) =>
              State.modify[ScanState](s => s.copy(seenVars = s.seenVars ++ whenTrueWitness.toSet[ec.Var] ++ whenFalseWitness.toSet[ec.Var]))
                *> exprBaseScanner.scan(a)

            case Expr.Variable(v) =>
              exprBaseScanner.scan(v.varType) *>
                State.modify[ScanState] { s =>
                  if s.seenVars.contains(v) then
                    s
                  else
                    s.copy(seenVars = s.seenVars + v, freeVars = s.freeVars :+ v)
                }

            case Expr.VariableStore(v, value) =>
              exprBaseScanner.scan(v.varType) *>
                State.modify[ScanState] { s =>
                  if s.seenVars.contains(v) then
                    s
                  else
                    s.copy(seenVars = s.seenVars + v, freeVars = s.freeVars :+ v)
                } *>
                exprBaseScanner.scan(value)

            case _ => exprBaseScanner.scan(a)
          }
      end exprScanner

      private val exprBaseScanner: Scanner[Expr] = autoScanner[Expr]
      
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

      private given Scanner[UniqueIdentifier] = IgnoreScanner[UniqueIdentifier]
      private given Scanner[IdentifierExpr] = IgnoreScanner[IdentifierExpr]
      private given Scanner[Boolean] = IgnoreScanner[Boolean]
      private given Scanner[BigInt] = IgnoreScanner[BigInt]
      private given Scanner[Int] = IgnoreScanner[Int]
      private given Scanner[String] = IgnoreScanner[String]
    }

    val res = scanner.exprScanner.scan(expr).runF.value(ScanState(Set.empty, Seq.empty)).value._1

    res.freeVars
  end apply
}
