package dev.argon.prover.prolog

import dev.argon.prover.*
import zio.*
import zio.test.*
import zio.test.Assertion.*
import SimplePrologContext.VariableProvider

object PrologListTests extends ZIOSpecDefault {

  sealed trait TestPredicate derives CanEqual
  case object Member extends TestPredicate derives CanEqual

  sealed trait TestCtor derives CanEqual
  case object Cons extends TestCtor derives CanEqual
  case object Nil extends TestCtor derives CanEqual
  case object A extends TestCtor derives CanEqual
  case object B extends TestCtor derives CanEqual

  val prologSyntax = new SimpleProverSyntax[TestPredicate, TestCtor]
  import prologSyntax.*

  val fuel = 100

  private object FuelContext extends SimplePrologContext[VariableProvider, Nothing] {
    override val syntax: prologSyntax.type = prologSyntax

    protected override val assertions: URIO[VariableProvider, List[(Proof[Unit], Predicate)]] =
      ZIO.foreach(List(
        (for {
          x <- VariableProvider.next
          t <- VariableProvider.next
        } yield pred(Member, v"$x", expr(Cons, v"$x", v"$t"))),
        (for {
          x <- VariableProvider.next
          y <- VariableProvider.next
          t <- VariableProvider.next
        } yield pred(Member, v"$x", v"$t") ==> pred(Member, v"$x", expr(Cons, v"$y", v"$t"))),
      )) { predicateIO => predicateIO.map { Proof.Atomic(()) -> _ } }

  }

  private val prologContext = FuelContext
  import prologContext.ProofResult

  override def spec: Spec[Environment & Scope, Any] =
    suite("Lists")(
      test("member with head value (size=1)") {
        assertZIO(prologContext.check(pred(Member, expr(A), expr(Cons, expr(A), expr(Nil))), fuel))(
          isSubtype[ProofResult.Yes](anything)
        )
      },
      test("member with head value (size=2)") {
        assertZIO(prologContext.check(pred(Member, expr(A), expr(Cons, expr(A), expr(Cons, expr(B), expr(Nil)))), fuel))(
          isSubtype[ProofResult.Yes](anything)
        )
      },
      test("member with non-head value (size=2)") {
        assertZIO(prologContext.check(pred(Member, expr(A), expr(Cons, expr(B), expr(Cons, expr(A), expr(Nil)))), fuel))(
          isSubtype[ProofResult.Yes](anything)
        )
      },
      test("member with non-head value (size=3)") {
        assertZIO(prologContext.check(
          pred(Member, expr(A), expr(Cons, expr(B), expr(Cons, expr(B), expr(Cons, expr(A), expr(Nil))))),
          fuel,
        ))(isSubtype[ProofResult.Yes](anything))
      },
      test("member with missing value (size=1)") {
        assertZIO(prologContext.check(pred(Member, expr(A), expr(Cons, expr(B), expr(Nil))), fuel))(equalTo(
          ProofResult.Unknown
        ))
      },
      test("member with missing value (size=2)") {
        assertZIO(prologContext.check(pred(Member, expr(A), expr(Cons, expr(B), expr(Cons, expr(B), expr(Nil)))), fuel))(
          equalTo(ProofResult.Unknown)
        )
      },
    ).provideSome[Environment](VariableProvider.live)

}
