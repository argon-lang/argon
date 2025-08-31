package dev.argon.prover.smt

import dev.argon.prover.*
import dev.argon.util.Fuel
import zio.*
import zio.test.*
import zio.test.Assertion.*

object SmtOrderTests extends ZIOSpecDefault {

  sealed trait TestCtor derives CanEqual
  case object EqualTo extends TestCtor derives CanEqual
  case object Not extends TestCtor derives CanEqual
  case object Lt extends TestCtor derives CanEqual
  case object Le extends TestCtor derives CanEqual
  case object Gt extends TestCtor derives CanEqual
  case object Ge extends TestCtor derives CanEqual
  case object TrueValue extends TestCtor derives CanEqual
  case object FalseValue extends TestCtor derives CanEqual
  case object IntType extends TestCtor derives CanEqual
  case object BoolType extends TestCtor derives CanEqual

  val dummyProof = Proof.Atomic("dummy")

  private val prologSyntax: SimpleProverSyntax[TestCtor] = new SimpleProverSyntax[TestCtor]
  import prologSyntax.*


  private final class TestContext(extraAssertions: Seq[Predicate]) extends TestSmtContext[TestCtor] {
    override val syntax: prologSyntax.type = prologSyntax

    private val commonAssertions = Seq()

    override val fuel = Fuel(4)

    override protected val theories: Seq[Theory] = Seq.empty

    protected override def freshAssertions(model: Model): Seq[URIO[VariableProvider, TVariable] => URIO[VariableProvider, (Proof[String], Predicate)]] =
      (commonAssertions ++ extraAssertions)
        .map { p =>
          (_: URIO[VariableProvider, TVariable]) => ZIO.succeed(dummyProof -> p)
        } ++
        Seq(
          (newVariable: URIO[VariableProvider, TVariable]) => (
            for {
              t <- newVariable
              n <- newVariable
            } yield dummyProof -> pred(EqualTo, t, n, n)
          ),
          (newVariable: URIO[VariableProvider, TVariable]) => (
            for {
              t <- newVariable
              a <- newVariable
              b <- newVariable
            } yield dummyProof -> Implies(
              pred(EqualTo, t, a, b),
              pred(EqualTo, t, b, a)
            )
          ),

          // (not a == true) -> a == false, etc
          (newVariable: URIO[VariableProvider, TVariable]) => (
            for {
              a <- newVariable
            } yield dummyProof -> Implies(
              !pred(EqualTo, expr(BoolType), a, expr(TrueValue)),
              pred(EqualTo, expr(BoolType), a, expr(FalseValue))
              )),
          (newVariable: URIO[VariableProvider, TVariable]) => (
            for {
              a <- newVariable
            } yield dummyProof -> Implies(
              pred(EqualTo, expr(BoolType), a, expr(TrueValue)),
              !pred(EqualTo, expr(BoolType), a, expr(FalseValue))
            )),
          (newVariable: URIO[VariableProvider, TVariable]) => (
            for {
              a <- newVariable
            } yield dummyProof -> Implies(
              !pred(EqualTo, expr(BoolType), a, expr(FalseValue)),
              pred(EqualTo, expr(BoolType), a, expr(TrueValue))
            )),
          (newVariable: URIO[VariableProvider, TVariable]) => (
            for {
              a <- newVariable
            } yield dummyProof -> Implies(
              pred(EqualTo, expr(BoolType), a, expr(FalseValue)),
              !pred(EqualTo, expr(BoolType), a, expr(TrueValue))
            )),

          // a == true or a == false
          (newVariable: URIO[VariableProvider, TVariable]) => (
            for {
              n <- newVariable
            } yield dummyProof -> Or(
              pred(EqualTo, expr(BoolType), n, expr(TrueValue)),
              pred(EqualTo, expr(BoolType), n, expr(FalseValue)),
            )
          ),

          // n == m -> (n <= m) == true
          (newVariable: URIO[VariableProvider, TVariable]) => (
            for {
              n <- newVariable
              m <- newVariable
            } yield dummyProof -> Implies(
              pred(EqualTo, expr(IntType), n, m),
              pred(Le, n, m)
            )
          ),

          // (a <= b) -> (a == b) \/ (a < b)
          (newVariable: URIO[VariableProvider, TVariable]) => (
            for {
              n <- newVariable
              m <- newVariable
            } yield dummyProof -> Implies(
              pred(Le, n, m),
              pred(EqualTo, expr(IntType), n, m) |
                pred(Lt, n, m)
            )
          ),

          // a <= b or b <= a
          (newVariable: URIO[VariableProvider, TVariable]) => (
            for {
              a <- newVariable
              b <- newVariable
            } yield dummyProof -> (
              pred(Le, a, b) |
                pred(Le, b, a)
            )
          ),
        )

  }

  private def smtContext(extraAssertions: Predicate*): TestContext = new TestContext(extraAssertions)

  override def spec: Spec[Environment & Scope, Any] =
    suite("SMT Order Proofs")(
      test("prove x == x") {
        smtContext().assertProves(pred(EqualTo, expr(IntType), Expr.Variable("x"), Expr.Variable("x")))
      },
      test("prove x == y given y == x") {
        smtContext(
          pred(EqualTo, expr(IntType), Expr.Variable("y"), Expr.Variable("x")),
        ).assertProves(pred(EqualTo, expr(IntType), Expr.Variable("x"), Expr.Variable("y")))
      },
      test("prove not x == y given not y == x") {
        smtContext(
          !pred(EqualTo, expr(IntType), Expr.Variable("y"), Expr.Variable("x")),
        ).assertProves(!pred(EqualTo, expr(IntType), Expr.Variable("x"), Expr.Variable("y")))
      },
      test("prove not x <= y given not x == y, not x < y") {
        smtContext(
          !pred(EqualTo, expr(IntType), Expr.Variable("x"), Expr.Variable("y")),
          !pred(Lt, Expr.Variable("x"), Expr.Variable("y")),
        ).assertProves(!pred(Le, Expr.Variable("x"), Expr.Variable("y")))
      },
      test("prove y <= x given not x <= y") {
        smtContext(
          !pred(Le, Expr.Variable("x"), Expr.Variable("y")),
        ).assertProves(pred(Le, Expr.Variable("y"), Expr.Variable("x")))
      },
    ).provide(VariableProvider.live)

}
