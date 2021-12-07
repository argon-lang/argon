package dev.argon.prover

import dev.argon.prover.SimplePrologContext.VariableProvider
import zio.{URIO, ZIO}
import zio.test.Assertion._
import zio.test._

object PrologFuelTests extends DefaultRunnableSpec {

  sealed trait TestPredicate derives CanEqual
  case object Infinite extends TestPredicate derives CanEqual

  sealed trait TestCtor derives CanEqual
  case object A extends TestCtor derives CanEqual

  val prologSyntax = new SimplePrologSyntax[TestPredicate, TestCtor]
  import prologSyntax._

  val fuel = 100

  private object TestContext extends SimplePrologContext[VariableProvider & zio.Random, Nothing] {
    override val syntax: prologSyntax.type = prologSyntax

    override protected val assertions: URIO[VariableProvider, List[(Proof[Unit], Predicate)]] = ZIO.foreach(List(
      (for {
        x <- VariableProvider.next
      } yield pred(Infinite, v"$x") ==> pred(Infinite, v"$x")),
    )) { predicateIO => predicateIO.map { Proof.Atomic(()) -> _ } }
  }


  import TestContext.PrologResult

  override def spec: ZSpec[Environment, Failure] =
    suite("Fuel tests")(
      testM("recursive predicate") {
        assertM(TestContext.check(pred(Infinite, expr(A)), fuel))(equalTo(PrologResult.Unknown))
      },
    ).provideCustomLayer(VariableProvider.live)
}
