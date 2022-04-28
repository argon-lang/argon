package dev.argon.prover

import dev.argon.prover.SimplePrologContext.VariableProvider
import zio.*
import zio.test.Assertion.*
import zio.test.*

object PrologFuelTests extends ZIOSpecDefault {

  sealed trait TestPredicate derives CanEqual
  case object Infinite extends TestPredicate derives CanEqual

  sealed trait TestCtor derives CanEqual
  case object A extends TestCtor derives CanEqual

  val prologSyntax = new SimplePrologSyntax[TestPredicate, TestCtor]
  import prologSyntax.*

  val fuel = 100

  private object TestContext extends SimplePrologContext[VariableProvider, Nothing] {
    override val syntax: prologSyntax.type = prologSyntax

    protected override val assertions: URIO[VariableProvider, List[(Proof[Unit], Predicate)]] =
      ZIO.foreach(List(
        (for {
          x <- VariableProvider.next
        } yield pred(Infinite, v"$x") ==> pred(Infinite, v"$x"))
      )) { predicateIO => predicateIO.map { Proof.Atomic(()) -> _ } }

  }

  import TestContext.PrologResult

  override def spec: ZSpec[Environment & Scope, Any] =
    suite("Fuel tests")(
      test("recursive predicate") {
        assertM(TestContext.check(pred(Infinite, expr(A)), fuel))(equalTo(PrologResult.Unknown))
      }
    ).provideSome[Environment](VariableProvider.live)

}
