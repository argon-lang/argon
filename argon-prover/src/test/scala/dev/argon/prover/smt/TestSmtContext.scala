package dev.argon.prover.smt

import dev.argon.util.{*, given}
import dev.argon.prover.*
import zio.*
import zio.stream.{Stream, ZStream}

abstract class TestSmtContext[PredFunc, Constructor](using CanEqual[PredFunc, PredFunc], CanEqual[Constructor, Constructor])
    extends SmtContext[VariableProvider, Nothing]
      with TestProverContextBase[PredFunc, Constructor] {
  import syntax.*

  override type ProofAtom = String
  override type Expr = syntax.Expr

  override protected def variableToExpr(v: Expr.Variable): Expr = v

  override protected def assumeResultProof: Proof[String] = Proof.Atomic("dummy")

  override protected def normalizePredicateExpression(p: PredicateApply, model: Model, fuel: Fuel): ZIO[VariableProvider, Nothing, PredicateApply] =
    ZIO.succeed(p)

  override protected def substituteVariablesPE(varMap: Map[Expr.Variable, Expr])(pf: PredicateApply): PredicateApply =
    PredicateApply(pf.name, pf.args.map(substituteVariablesExpr(varMap)))
    
  private def substituteVariablesExpr(varMap: Map[Expr.Variable, Expr])(e: Expr): Expr =
    e match {
      case Expr.Value(ctor, args) =>
        Expr.Value(ctor, args.map(substituteVariablesExpr(varMap)))

      case v: Expr.Variable =>
        varMap.getOrElse(v, v)
    }

  override protected def matchPredicateExpr(a: PredicateApply, b: PredicateApply, state: ProverState, quantVars: Set[Expr.Variable]): ZIO[VariableProvider, Nothing, Option[Map[Expr.Variable, Expr]]] =
    for
      quantVarMap <- Ref.make(Map.empty[Expr.Variable, Expr])
      res <-
        ZIO.succeed(a.name == b.name && a.args.size == b.args.size) &&
          ZIO.forall(a.args.zip(b.args)) { (a, b) => exprEquiv(a, b, state, quantVars, quantVarMap) }
      qvm <- quantVarMap.get
    yield if res then Some(qvm) else None

  private def exprEquiv(a: Expr, b: Expr, state: ProverState, quantVars: Set[Expr.Variable], quantVarMap: Ref[Map[Expr.Variable, Expr]]): UIO[Boolean] =
    if state.fuel.isEmpty then
      ZIO.succeed(false)
    else
      (a, b) match {
        case (_, b: Expr.Variable) if quantVars.contains(b) =>
          quantVarMap.get.flatMap { qvm =>
            qvm.get(b) match {
              case Some(mappedExpr) =>
                exprEquiv(a, mappedExpr, state.consumeFuel, quantVars, quantVarMap)

              case None =>
                quantVarMap.set(qvm.updated(b, a)).as(true)
            }
          }

        case (Expr.Value(c1, args1), Expr.Value(c2, args2)) =>
          ZIO.succeed(c1 == c2 && args1.size == args2.size) &&
            ZIO.forall(args1.zip(args2)) { (a, b) => exprEquiv(a, b, state, quantVars, quantVarMap) }

        case (Expr.Variable(v1), Expr.Variable(v2)) if v1 == v2 => ZIO.succeed(true)

        case (Expr.Variable(v1), _) =>
          state.model.get(v1) match {
            case Some(a2) => exprEquiv(a2, b, state.consumeFuel, quantVars, quantVarMap)
            case None => ZIO.succeed(false)
          }

        case (_, Expr.Variable(v2)) =>
          state.model.get(v2) match {
            case Some(b2) => exprEquiv(a, b2, state.consumeFuel, quantVars, quantVarMap)
            case None => ZIO.succeed(false)
          }
      }
}
