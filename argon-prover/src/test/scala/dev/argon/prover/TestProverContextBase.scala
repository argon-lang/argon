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

trait TestProverContextBase[PredFunc, Constructor](using CanEqual[PredFunc, PredFunc], CanEqual[Constructor, Constructor])
    extends ProverContext[VariableProvider, Nothing] {
  override val syntax: SimpleProverSyntax[PredFunc, Constructor]
  import syntax.*

  override type ProofAtom = String
  override type Model = Map[String, Expr]

  val fuel: Int

  protected final override def newVariable: URIO[VariableProvider, Expr.Variable] =
    VariableProvider.next.map(Expr.Variable.apply)


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
