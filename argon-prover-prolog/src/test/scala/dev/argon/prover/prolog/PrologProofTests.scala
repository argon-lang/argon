package dev.argon.prover.prolog

import dev.argon.prover.prolog.SimplePrologContext.VariableProvider
import dev.argon.prover.*
import zio.*
import zio.test.Assertion.*
import zio.test.*

object PrologProofTests extends ZIOSpecDefault {

  sealed trait TestPredicate derives CanEqual
  case object Gt extends TestPredicate derives CanEqual
  case object KnownTrue extends TestPredicate derives CanEqual
  case object KnownFalse extends TestPredicate derives CanEqual
  case object KnownDisjunctLeft extends TestPredicate derives CanEqual
  case object KnownDisjunctRight extends TestPredicate derives CanEqual

  sealed trait TestCtor derives CanEqual
  case object Succ extends TestCtor derives CanEqual
  case object Zero extends TestCtor derives CanEqual

  val prologSyntax = new SimpleProverSyntax[TestPredicate, TestCtor]
  import prologSyntax.*

  val fuel = 100

  private final class TestContext
      extends ProofPrologContext[VariableProvider, Nothing]
      with ProofAssertions[VariableProvider, Nothing] {
    override val syntax: prologSyntax.type = prologSyntax

    val succIsGreaterThanZero = Proof.Atomic("succIsGreaterThanZero")
    val succIsGreaterThanSucc = Proof.Atomic("succIsGreaterThanSucc")
    val knownDisjunct = Proof.Atomic("knownDisjunct")
    val knownTrue = Proof.Atomic("knownTrue")
    val knownFalse = Proof.Atomic("knownFalse")
    

    protected override def assertions: Seq[URIO[VariableProvider, TVariable] => URIO[VariableProvider, (Proof[String], Predicate)]] =
      Seq[(Proof[String], URIO[VariableProvider, TVariable] => URIO[VariableProvider, Predicate])](
        succIsGreaterThanZero -> (newVariable => for {
          x <- newVariable
        } yield pred(Gt, expr(Succ, v"$x"), expr(Zero))),
        succIsGreaterThanSucc -> (newVariable => for {
          x <- newVariable
          y <- newVariable
        } yield pred(Gt, v"$x", v"$y") ==> pred(Gt, expr(Succ, v"$x"), expr(Succ, v"$y"))),
        knownDisjunct -> (_ => ZIO.succeed(Or(pred(KnownDisjunctLeft), pred(KnownDisjunctRight)))),
        knownTrue -> (_ => ZIO.succeed(pred(KnownTrue))),
        knownFalse -> (_ => ZIO.succeed(not(pred(KnownFalse)))),
      ).map { case (proof, predicateIO) => newVariable => predicateIO(newVariable).map { proof -> _ } }

  }

  private val prologContext: TestContext = new TestContext

  import prologContext.{check as _, *}

  override def spec: Spec[Environment & Scope, Any] =
    suite("Proofs")(
      test("1 > 0") {
        assertZIO(prologContext.check(pred(Gt, expr(Succ, expr(Zero)), expr(Zero)), fuel))(hasProof(succIsGreaterThanZero))
      },
      test("2 > 1") {
        assertZIO(prologContext.check(pred(Gt, expr(Succ, expr(Succ, expr(Zero))), expr(Succ, expr(Zero))), fuel))(
          hasProof(Proof.ModusPonens(succIsGreaterThanSucc, succIsGreaterThanZero))
        )
      },
      test("3 > 1") {
        assertZIO(prologContext.check(
          pred(Gt, expr(Succ, expr(Succ, expr(Succ, expr(Zero)))), expr(Succ, expr(Zero))),
          fuel,
        ))(hasProof(Proof.ModusPonens(succIsGreaterThanSucc, succIsGreaterThanZero)))
      },
      test("not 0 > 0") {
        assertZIO(prologContext.check(pred(Gt, expr(Zero), expr(Zero)), fuel))(equalTo(ProofResult.Unknown))
      },
      test("not 1 > 1") {
        assertZIO(prologContext.check(pred(Gt, expr(Succ, expr(Zero)), expr(Succ, expr(Zero))), fuel))(equalTo(
          ProofResult.Unknown
        ))
      },
      test("not 1 > 2") {
        assertZIO(prologContext.check(pred(Gt, expr(Succ, expr(Zero)), expr(Succ, expr(Succ, expr(Zero)))), fuel))(equalTo(
          ProofResult.Unknown
        ))
      },
      test("true") {
        assertZIO(prologContext.check(pred(KnownTrue), fuel))(
          hasProof(knownTrue)
        )
      },
      test("false") {
        assertZIO(prologContext.check(pred(KnownFalse), fuel))(disproved(equalTo(knownFalse)))
      },
      test("disjunct") {
        assertZIO(prologContext.check(Or(pred(KnownDisjunctLeft), pred(KnownDisjunctRight)), fuel))(hasProof(knownDisjunct))
      },
      test("disjunct left") {
        assertZIO(prologContext.check(Or(pred(KnownTrue), pred(KnownFalse)), fuel))(
          hasProof(Proof.DisjunctIntroLeft(knownTrue))
        )
      },
      test("disjunct right") {
        assertZIO(prologContext.check(Or(pred(KnownFalse), pred(KnownTrue)), fuel))(
          hasProof(Proof.DisjunctIntroRight(knownTrue))
        )
      },
      test("not (true | false)") {
        assertZIO(prologContext.check(not(Or(pred(KnownTrue), pred(KnownFalse))), fuel))(notProven)
      },
      test("not (false | true)") {
        assertZIO(prologContext.check(not(Or(pred(KnownFalse), pred(KnownTrue))), fuel))(notProven)
      },
      test("not (true | true)") {
        assertZIO(prologContext.check(not(Or(pred(KnownTrue), pred(KnownTrue))), fuel))(notProven)
      },
      test("not (true & true)") {
        assertZIO(prologContext.check(not(And(pred(KnownTrue), pred(KnownTrue))), fuel))(equalTo(ProofResult.Unknown))
      },
      test("not not true") {
        assertZIO(prologContext.check(not(not(pred(KnownTrue))), fuel))(
          hasProof(Proof.DoubleNegIntro(knownTrue))
        )
      },
    ).provideSome[Environment](VariableProvider.live)

}
