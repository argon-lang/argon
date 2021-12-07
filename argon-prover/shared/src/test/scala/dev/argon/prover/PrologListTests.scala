package dev.argon.prover

import zio.{URIO, ZIO}
import zio.test._
import zio.test.Assertion._
import SimplePrologContext.VariableProvider

object PrologListTests extends DefaultRunnableSpec {

  sealed trait TestPredicate derives CanEqual
  case object Member extends TestPredicate derives CanEqual

  sealed trait TestCtor derives CanEqual
  case object Cons extends TestCtor derives CanEqual
  case object Nil extends TestCtor derives CanEqual
  case object A extends TestCtor derives CanEqual
  case object B extends TestCtor derives CanEqual

  val prologSyntax = new SimplePrologSyntax[TestPredicate, TestCtor]
  import prologSyntax._

  val fuel = 100

  private object FuelContext extends SimplePrologContext[VariableProvider & zio.Random, Nothing] {
    override val syntax: prologSyntax.type = prologSyntax

    override protected val assertions: URIO[VariableProvider, List[(Proof[Unit], Predicate)]] = ZIO.foreach(List(
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
  import prologContext.PrologResult

  override def spec: ZSpec[Environment, Failure] =
    suite("Lists")(
      testM("member with head value (size=1)") {
        assertM(prologContext.check(pred(Member, expr(A), expr(Cons, expr(A), expr(Nil))), fuel))(isSubtype[PrologResult.Yes](anything))
      },
      testM("member with head value (size=2)") {
        assertM(prologContext.check(pred(Member, expr(A), expr(Cons, expr(A), expr(Cons, expr(B), expr(Nil)))), fuel))(isSubtype[PrologResult.Yes](anything))
      },
      testM("member with non-head value (size=2)") {
        assertM(prologContext.check(pred(Member, expr(A), expr(Cons, expr(B), expr(Cons, expr(A), expr(Nil)))), fuel))(isSubtype[PrologResult.Yes](anything))
      },
      testM("member with non-head value (size=3)") {
        assertM(prologContext.check(pred(Member, expr(A), expr(Cons, expr(B), expr(Cons, expr(B), expr(Cons, expr(A), expr(Nil))))), fuel))(isSubtype[PrologResult.Yes](anything))
      },
      testM("member with missing value (size=1)") {
        assertM(prologContext.check(pred(Member, expr(A), expr(Cons, expr(B), expr(Nil))), fuel))(equalTo(PrologResult.Unknown))
      },
      testM("member with missing value (size=2)") {
        assertM(prologContext.check(pred(Member, expr(A), expr(Cons, expr(B), expr(Cons, expr(B), expr(Nil)))), fuel))(equalTo(PrologResult.Unknown))
      },
    ).provideCustomLayer(VariableProvider.live)
}
