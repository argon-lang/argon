package dev.argon.prover.smt

import dev.argon.util.{*, given}
import dev.argon.prover.*
import zio.*
import zio.stream.{Stream, ZStream}
import dev.argon.util.NothingTypeTest.given

abstract class TestSmtContext[Constructor](using CanEqual[Constructor, Constructor])
    extends SmtContext[VariableProvider, Nothing]
      with TestProverContextBase[Constructor] {
  import syntax.*

  override type ProofAtom = String

  override protected def assumeResultProof: Proof[String] = Proof.Atomic("dummy")

  override protected def normalizePredicateExpression(p: Expr, model: Model, fuel: Fuel): ZIO[VariableProvider, Nothing, Expr] =
    ZIO.succeed {
      p match {
        case Expr.Variable(name) =>
          model.get(name).getOrElse(p)

        case Expr.Value(_, _) => p
      }
    }

  override protected def predicateReferencesVariable(p: Expr, v: Expr.Variable): Boolean =
    p match
      case Expr.Variable(name) => v.name == name
      case Expr.Value(constructor, args) => args.exists(predicateReferencesVariable(_, v))
    end match

  override protected def substituteVariablesPE(varMap: Map[Expr.Variable, Expr])(pe: Expr): Expr =
    pe match {
      case Expr.Value(ctor, args) =>
        Expr.Value(ctor, args.map(substituteVariablesPE(varMap)))

      case v: Expr.Variable =>
        varMap.getOrElse(v, v)
    }

  override protected def matchPredicateExpr(a: Expr, b: Expr, state: ProverState, quantVars: Set[Expr.Variable]): UIO[Option[Map[Expr.Variable, Expr]]] =
    for
      quantVarMap <- Ref.make(Map.empty[Expr.Variable, Expr])
      res <- exprEquiv(a, b, state, quantVars, quantVarMap)
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
