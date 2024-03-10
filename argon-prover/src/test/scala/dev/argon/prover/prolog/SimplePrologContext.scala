package dev.argon.prover.prolog

import cats.data.OptionT
import dev.argon.util.{*, given}
import dev.argon.prover.*
import dev.argon.prover.prolog.PrologContext
import zio.*
import zio.stream.{Stream, ZStream}
import zio.test.Assertion
import cats.*
import cats.implicits.given
import zio.interop.catz.core.*

abstract class SimplePrologContext[PredFunc, Constructor](using CanEqual[PredFunc, PredFunc], CanEqual[Constructor, Constructor])
    extends PrologContext[VariableProvider, Nothing] with TestProverContextBase[PredFunc, Constructor] {
  override val syntax: SimpleProverSyntax[PredFunc, Constructor]
  import syntax.*

  override protected def intrinsicPredicate(predicate: PredicateApply, model: Model, solveState: SolveState): ZStream[VariableProvider, Nothing, ProofResult.Definitive] =
    ZStream.empty

  override protected def unifyPredicateExpression(f1: PredicateApply, f2: PredicateApply, model: Ref[Model], fuel: Fuel): ZIO[VariableProvider, Nothing, Boolean] =
    ZIO.succeed(f1.name == f2.name && f1.args.length == f2.args.length) &&
      f1.args.zip(f2.args).forallM { case (a, b) => unifyExpr(a, b, model, fuel) }

  private def unifyExpr(a: Expr, b: Expr, model: Ref[Model], fuel: Fuel): UIO[Boolean] =
    if fuel.isEmpty then
      ZIO.succeed(false)
    else
      (a, b) match {
        case (Expr.Value(c1, args1), Expr.Value(c2, args2)) =>
          ZIO.succeed(c1 == c2 && args1.size == args2.size) &&
            args1.zip(args2).forallM { case (a, b) => unifyExpr(a, b, model, fuel) }

        case (Expr.Variable(va), Expr.Variable(vb)) if va == vb =>
          ZIO.succeed(true)

        case (Expr.Variable(va), Expr.Variable(vb)) =>
          model.get.flatMap { m =>
            if m.contains(va) || !m.contains(vb) then
              unifyVariable(va, b, model, fuel)
            else
              unifyVariable(vb, a, model, fuel)
          }

        case (Expr.Variable(va), _) =>
          unifyVariable(va, b, model, fuel)

        case (_, Expr.Variable(vb)) =>
          unifyVariable(vb, a, model, fuel)
      }

  private def unifyVariable(a: String, b: Expr, model: Ref[Model], fuel: Fuel): UIO[Boolean] =
    model.get.flatMap { m =>
      m.get(a) match {
        case Some(a2) => unifyExpr(a2, b, model, fuel.consume)
        case None =>
          model.set(m.updated(a, b)).as(true)
      }
    }

}
