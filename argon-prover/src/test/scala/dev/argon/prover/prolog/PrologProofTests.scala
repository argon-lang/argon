package dev.argon.prover.prolog

import dev.argon.prover.*
import zio.*
import zio.test.Assertion.*
import zio.test.*
import cats.*
import cats.implicits.given
import dev.argon.util.Fuel
import zio.interop.catz.core.*

object PrologProofTests extends ZIOSpecDefault {

  sealed trait TestCtor derives CanEqual
  case object Gt extends TestCtor derives CanEqual
  case object KnownTrue extends TestCtor derives CanEqual
  case object KnownFalse extends TestCtor derives CanEqual
  case object KnownDisjunctLeft extends TestCtor derives CanEqual
  case object KnownDisjunctRight extends TestCtor derives CanEqual
  case object Succ extends TestCtor derives CanEqual
  case object Zero extends TestCtor derives CanEqual

  val prologSyntax = new SimpleProverSyntax[TestCtor]
  import prologSyntax.*

  private final class TestContext extends SimplePrologContext[TestCtor] {
    override val syntax: prologSyntax.type = prologSyntax
    
    override val fuel: Fuel = Fuel(100)

    val succIsGreaterThanZero = Proof.Atomic("succIsGreaterThanZero")
    val succIsGreaterThanSucc = Proof.Atomic("succIsGreaterThanSucc")
    val knownDisjunct = Proof.Atomic("knownDisjunct")
    val knownTrue = Proof.Atomic("knownTrue")
    val knownFalse = Proof.Atomic("knownFalse")

    override protected def freshAssertions(model: Model): Seq[URIO[VariableProvider, TVariable] => URIO[VariableProvider, (Proof[ProofAtom], Predicate)]] =
      Seq(
        newVariable => for {
          x <- newVariable
        } yield succIsGreaterThanZero -> pred(Gt, expr(Succ, x), expr(Zero)),

        newVariable => for {
          x <- newVariable
          y <- newVariable
        } yield succIsGreaterThanSucc -> (pred(Gt, x, y) ==> pred(Gt, expr(Succ, x), expr(Succ, y))),

        newVariable => ZIO.succeed(knownDisjunct -> Or(pred(KnownDisjunctLeft), pred(KnownDisjunctRight))),
        newVariable => ZIO.succeed(knownTrue -> pred(KnownTrue)),
        newVariable => ZIO.succeed(knownFalse -> !pred(KnownFalse)),
      )

  }

  private val prologContext: TestContext = new TestContext

  import prologContext.{check as _, *}

  override def spec: Spec[Environment & Scope, Any] =
    suite("Prolog Proofs")(
      test("1 > 0") {
        assertZIO(prologContext.check(pred(Gt, expr(Succ, expr(Zero)), expr(Zero)), Map.empty, fuel))(hasProof(succIsGreaterThanZero))
      },
      test("2 > 1") {
        assertZIO(prologContext.check(pred(Gt, expr(Succ, expr(Succ, expr(Zero))), expr(Succ, expr(Zero))), Map.empty, fuel))(
          hasProof(Proof.ModusPonens(succIsGreaterThanSucc, succIsGreaterThanZero))
        )
      },
      test("3 > 1") {
        assertZIO(prologContext.check(
          pred(Gt, expr(Succ, expr(Succ, expr(Succ, expr(Zero)))), expr(Succ, expr(Zero))),
          Map.empty,
          fuel,
        ))(hasProof(Proof.ModusPonens(succIsGreaterThanSucc, succIsGreaterThanZero)))
      },
      test("not 0 > 0") {
        assertZIO(prologContext.check(pred(Gt, expr(Zero), expr(Zero)), Map.empty, fuel))(equalTo(ProofResult.Unknown))
      },
      test("not 1 > 1") {
        assertZIO(prologContext.check(pred(Gt, expr(Succ, expr(Zero)), expr(Succ, expr(Zero))), Map.empty, fuel))(equalTo(
          ProofResult.Unknown
        ))
      },
      test("not 1 > 2") {
        assertZIO(prologContext.check(pred(Gt, expr(Succ, expr(Zero)), expr(Succ, expr(Succ, expr(Zero)))), Map.empty, fuel))(equalTo(
          ProofResult.Unknown
        ))
      },
      test("true") {
        assertZIO(prologContext.check(pred(KnownTrue), Map.empty, fuel))(
          hasProof(knownTrue)
        )
      },
      test("false") {
        assertZIO(prologContext.check(pred(KnownFalse), Map.empty, fuel))(disproved(equalTo(knownFalse)))
      },
      test("disjunct") {
        assertZIO(prologContext.check(Or(pred(KnownDisjunctLeft), pred(KnownDisjunctRight)), Map.empty, fuel))(hasProof(knownDisjunct))
      },
      test("disjunct left") {
        assertZIO(prologContext.check(Or(pred(KnownTrue), pred(KnownFalse)), Map.empty, fuel))(
          hasProof(Proof.DisjunctIntroLeft(knownTrue))
        )
      },
      test("disjunct right") {
        assertZIO(prologContext.check(Or(pred(KnownFalse), pred(KnownTrue)), Map.empty, fuel))(
          hasProof(Proof.DisjunctIntroRight(knownTrue))
        )
      },
      test("not (true | false)") {
        assertZIO(prologContext.check(!(pred(KnownTrue) | pred(KnownFalse)), Map.empty, fuel))(notProven)
      },
      test("not (false | true)") {
        assertZIO(prologContext.check(!(pred(KnownFalse) | pred(KnownTrue)), Map.empty, fuel))(notProven)
      },
      test("not (true | true)") {
        assertZIO(prologContext.check(!(pred(KnownTrue) | pred(KnownTrue)), Map.empty, fuel))(notProven)
      },
      test("not (true & true)") {
        assertZIO(prologContext.check(!(pred(KnownTrue) & pred(KnownTrue)), Map.empty, fuel))(equalTo(ProofResult.Unknown))
      },
      test("not not true") {
        assertZIO(prologContext.check(!(!pred(KnownTrue)), Map.empty, fuel))(
          hasProof(Proof.DoubleNegIntro(knownTrue))
        )
      },
    ).provideSome[Environment](VariableProvider.live)

}
