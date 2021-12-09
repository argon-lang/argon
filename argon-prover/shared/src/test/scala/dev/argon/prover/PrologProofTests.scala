package dev.argon.prover

import dev.argon.prover.SimplePrologContext.VariableProvider
import zio.{ZIO, IO, URIO}
import zio.test.Assertion.*
import zio.test.*

object PrologProofTests extends DefaultRunnableSpec {

  sealed trait TestPredicate derives CanEqual
  case object Gt extends TestPredicate derives CanEqual
  case object KnownTrue extends TestPredicate derives CanEqual
  case object KnownFalse extends TestPredicate derives CanEqual
  case object KnownDisjunctLeft extends TestPredicate derives CanEqual
  case object KnownDisjunctRight extends TestPredicate derives CanEqual

  sealed trait TestCtor derives CanEqual
  case object Succ extends TestCtor derives CanEqual
  case object Zero extends TestCtor derives CanEqual

  val prologSyntax = new SimplePrologSyntax[TestPredicate, TestCtor]
  import prologSyntax.*

  val fuel = 100

  private final class TestContext
      extends ProofPrologContext[VariableProvider & zio.Random, Nothing]
      with ProofAssertions[VariableProvider & zio.Random, Nothing] {
    override val syntax: prologSyntax.type = prologSyntax

    val succIsGreaterThanZero = Proof.Atomic("succIsGreaterThanZero")
    val succIsGreaterThanSucc = Proof.Atomic("succIsGreaterThanSucc")
    val knownDisjunct = Proof.Atomic("knownDisjunct")
    val knownTrue = Proof.Atomic("knownTrue")
    val knownFalse = Proof.Atomic("knownFalse")

    protected override def assertions: URIO[VariableProvider, List[(Proof[String], Predicate)]] =
      ZIO.foreach(List(
        succIsGreaterThanZero -> (for {
          x <- VariableProvider.next
        } yield pred(Gt, expr(Succ, v"$x"), expr(Zero))),
        succIsGreaterThanSucc -> (for {
          x <- VariableProvider.next
          y <- VariableProvider.next
        } yield pred(Gt, v"$x", v"$y") ==> pred(Gt, expr(Succ, v"$x"), expr(Succ, v"$y"))),
        knownDisjunct -> IO.succeed(Or(pred(KnownDisjunctLeft), pred(KnownDisjunctRight))),
        knownTrue -> IO.succeed(pred(KnownTrue)),
        knownFalse -> IO.succeed(not(pred(KnownFalse))),
      )) { case (proof, predicateIO) => predicateIO.map { proof -> _ } }

  }

  private val prologContext: TestContext = new TestContext

  import prologContext.{check as _, *}

  override def spec: ZSpec[Environment, Failure] =
    suite("Proofs")(
      testM("1 > 0") {
        assertM(prologContext.check(pred(Gt, expr(Succ, expr(Zero)), expr(Zero)), fuel))(hasProof(succIsGreaterThanZero))
      },
      testM("2 > 1") {
        assertM(prologContext.check(pred(Gt, expr(Succ, expr(Succ, expr(Zero))), expr(Succ, expr(Zero))), fuel))(
          hasProof(Proof.ModusPonens(succIsGreaterThanSucc, succIsGreaterThanZero))
        )
      },
      testM("3 > 1") {
        assertM(prologContext.check(
          pred(Gt, expr(Succ, expr(Succ, expr(Succ, expr(Zero)))), expr(Succ, expr(Zero))),
          fuel,
        ))(hasProof(Proof.ModusPonens(succIsGreaterThanSucc, succIsGreaterThanZero)))
      },
      testM("not 0 > 0") {
        assertM(prologContext.check(pred(Gt, expr(Zero), expr(Zero)), fuel))(equalTo(PrologResult.Unknown))
      },
      testM("not 1 > 1") {
        assertM(prologContext.check(pred(Gt, expr(Succ, expr(Zero)), expr(Succ, expr(Zero))), fuel))(equalTo(
          PrologResult.Unknown
        ))
      },
      testM("not 1 > 2") {
        assertM(prologContext.check(pred(Gt, expr(Succ, expr(Zero)), expr(Succ, expr(Succ, expr(Zero)))), fuel))(equalTo(
          PrologResult.Unknown
        ))
      },
      testM("true") {
        assertM(prologContext.check(pred(KnownTrue), fuel))(
          hasProof(knownTrue)
        )
      },
      testM("false") {
        assertM(prologContext.check(pred(KnownFalse), fuel))(disproved(equalTo(knownFalse)))
      },
      testM("disjunct") {
        assertM(prologContext.check(Or(pred(KnownDisjunctLeft), pred(KnownDisjunctRight)), fuel))(hasProof(knownDisjunct))
      },
      testM("disjunct left") {
        assertM(prologContext.check(Or(pred(KnownTrue), pred(KnownFalse)), fuel))(
          hasProof(Proof.DisjunctIntroLeft(knownTrue))
        )
      },
      testM("disjunct right") {
        assertM(prologContext.check(Or(pred(KnownFalse), pred(KnownTrue)), fuel))(
          hasProof(Proof.DisjunctIntroRight(knownTrue))
        )
      },
      testM("not false") {
        assertM(prologContext.check(not(pred(KnownFalse)), fuel))(hasProof(knownFalse))
      },
      testM("not (false | false)") {
        assertM(prologContext.check(not(Or(pred(KnownFalse), pred(KnownFalse))), fuel))(hasProof(
          Proof.DeMorganAndPullNotOut(Proof.ConjunctIntro(knownFalse, knownFalse))
        ))
      },
      testM("not (true | false)") {
        assertM(prologContext.check(not(Or(pred(KnownTrue), pred(KnownFalse))), fuel))(equalTo(PrologResult.Unknown))
      },
      testM("not (false | true)") {
        assertM(prologContext.check(not(Or(pred(KnownFalse), pred(KnownTrue))), fuel))(equalTo(PrologResult.Unknown))
      },
      testM("not (true | true)") {
        assertM(prologContext.check(not(Or(pred(KnownTrue), pred(KnownTrue))), fuel))(equalTo(PrologResult.Unknown))
      },
      testM("not (false & false)") {
        assertM(prologContext.check(not(And(pred(KnownFalse), pred(KnownFalse))), fuel))(
          hasProof(Proof.DeMorganOrPullNotOut(Proof.DisjunctIntroLeft(knownFalse))) ||
            hasProof(Proof.DeMorganOrPullNotOut(Proof.DisjunctIntroRight(knownFalse)))
        )
      },
      testM("not (false & true)") {
        assertM(prologContext.check(not(And(pred(KnownFalse), pred(KnownTrue))), fuel))(
          hasProof(Proof.DeMorganOrPullNotOut(Proof.DisjunctIntroLeft(knownFalse)))
        )
      },
      testM("not (true & false)") {
        assertM(prologContext.check(not(And(pred(KnownTrue), pred(KnownFalse))), fuel))(
          hasProof(Proof.DeMorganOrPullNotOut(Proof.DisjunctIntroRight(knownFalse)))
        )
      },
      testM("not (true & true)") {
        assertM(prologContext.check(not(And(pred(KnownTrue), pred(KnownTrue))), fuel))(equalTo(PrologResult.Unknown))
      },
      testM("not not true") {
        assertM(prologContext.check(not(not(pred(KnownTrue))), fuel))(
          hasProof(Proof.DoubleNegIntro(knownTrue))
        )
      },
    ).provideCustomLayer(VariableProvider.live)

}
