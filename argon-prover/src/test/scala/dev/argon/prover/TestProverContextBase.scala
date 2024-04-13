package dev.argon.prover

import cats.*
import cats.data.OptionT
import cats.implicits.given
import dev.argon.prover.*
import dev.argon.prover.prolog.PrologContext
import dev.argon.util.{*, given}
import zio.*
import zio.interop.catz.core.*
import zio.stream.{Stream, ZStream}
import zio.test.*

import scala.compiletime.{erasedValue, summonInline}

trait TestProverContextBase[Constructor](using CanEqual[Constructor, Constructor])
    extends ProverContext[VariableProvider, Nothing] {
  override val syntax: SimpleProverSyntax[Constructor]
  import syntax.*

  override type ProofAtom = String
  override type Model = Map[String, Expr]

  val fuel: Fuel

  protected final override def newVariable: URIO[VariableProvider, Expr.Variable] =
    VariableProvider.next.map(Expr.Variable.apply)

  override protected def unifyPredicateExpression(a: Expr, b: Expr, model: Ref[Model], fuel: Fuel): UIO[Boolean] =
    if fuel.isEmpty then
      ZIO.succeed(false)
    else
      (a, b) match {
        case (Expr.Value(c1, args1), Expr.Value(c2, args2)) =>
          ZIO.succeed(c1 == c2 && args1.size == args2.size) &&
            args1.zip(args2).forallM { case (a, b) => unifyPredicateExpression(a, b, model, fuel) }

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
        case Some(a2) => unifyPredicateExpression(a2, b, model, fuel.consume)
        case None =>
          model.set(m.updated(a, b)).as(true)
      }
    }

  def hasProof(proof: Proof[String]): Assertion[ProofResult] =
    Assertion.assertion[ProofResult]("hasProof") {
      case ProofResult.Yes(p, _) => proof == p
      case ProofResult.Unknown | ProofResult.No(_, _) => false
    }

  def isProven: Assertion[ProofResult] =
    Assertion.assertion[ProofResult]("hasProof") {
      case ProofResult.Yes(_, _) => true
      case ProofResult.Unknown | ProofResult.No(_, _) => false
    }

  def disproved(proof: Assertion[Proof[String]]): Assertion[ProofResult] =
    Assertion.assertionRec[ProofResult, Proof[String]]("disproved")(proof) {
      case ProofResult.No(p, _) => Some(p)
      case ProofResult.Unknown | ProofResult.Yes(_, _) => None
    }

  def notProven: Assertion[ProofResult] =
    Assertion.assertion[ProofResult]("notProven") {
      case ProofResult.No(_, _) | ProofResult.Unknown => true
      case ProofResult.Yes(_, _) => false
    }


  def assertProves(p: Predicate): ZIO[VariableProvider, Any, TestResult] =
    assertZIO(check(p, Map.empty, fuel))(isProven)

  def assertNotProves(p: Predicate): ZIO[VariableProvider, Any, TestResult] =
    assertZIO(check(p, Map.empty, fuel))(notProven)

}
