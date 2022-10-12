package dev.argon.prover.smt

import dev.argon.prover.*
import zio.*
import zio.test.*
import zio.test.Assertion.*

object SmtOrderTests extends ZIOSpecDefault {

  sealed trait TestPredicate derives CanEqual
  case object EqualTo extends TestPredicate derives CanEqual

  sealed trait TestCtor derives CanEqual
  case object Not extends TestCtor derives CanEqual
  case object Eq extends TestCtor derives CanEqual
  case object Lt extends TestCtor derives CanEqual
  case object Le extends TestCtor derives CanEqual
  case object Gt extends TestCtor derives CanEqual
  case object Ge extends TestCtor derives CanEqual
  case object TrueValue extends TestCtor derives CanEqual
  case object FalseValue extends TestCtor derives CanEqual
  case object IntType extends TestCtor derives CanEqual
  case object BoolType extends TestCtor derives CanEqual

  val dummyProof = Proof.Atomic("dummy")

  private val prologSyntax: SimpleProverSyntax[TestPredicate, TestCtor] = new SimpleProverSyntax[TestPredicate, TestCtor]
  import prologSyntax.*

  val fuel = 10

  private final class TestContext(extraAssertions: Seq[Predicate])
      extends TestSmtContext[VariableProvider, Nothing]
      with ProofAssertions[VariableProvider, Nothing] {
    override val syntax: prologSyntax.type = prologSyntax

    private val commonAssertions = Seq()

    protected override def assertions: Seq[URIO[VariableProvider, TVariable] => URIO[VariableProvider, (Proof[String], Predicate)]] =
      (commonAssertions ++ extraAssertions)
        .map { p =>
          (_: URIO[VariableProvider, TVariable]) => ZIO.succeed(dummyProof -> p)
        } ++
        Seq(
          (newVariable: URIO[VariableProvider, TVariable]) => (
            for {
              t <- newVariable
              n <- newVariable
            } yield dummyProof -> pred(EqualTo, Variable(t), Variable(n), Variable(n))
          ),
          (newVariable: URIO[VariableProvider, TVariable]) => (
            for {
              t <- newVariable
              a <- newVariable
              b <- newVariable
            } yield dummyProof -> Implies(
              pred(EqualTo, Variable(t), Variable(a), Variable(b)),
              pred(EqualTo, Variable(t), Variable(b), Variable(a))
            )
          ),

          // (not a == true) -> a == false, etc
          (newVariable: URIO[VariableProvider, TVariable]) => (
            for {
              a <- newVariable
            } yield dummyProof -> Implies(
              !pred(EqualTo, expr(BoolType), Variable(a), expr(TrueValue)),
              pred(EqualTo, expr(BoolType), Variable(a), expr(FalseValue))
              )),
          (newVariable: URIO[VariableProvider, TVariable]) => (
            for {
              a <- newVariable
            } yield dummyProof -> Implies(
              pred(EqualTo, expr(BoolType), Variable(a), expr(TrueValue)),
              !pred(EqualTo, expr(BoolType), Variable(a), expr(FalseValue))
            )),
          (newVariable: URIO[VariableProvider, TVariable]) => (
            for {
              a <- newVariable
            } yield dummyProof -> Implies(
              !pred(EqualTo, expr(BoolType), Variable(a), expr(FalseValue)),
              pred(EqualTo, expr(BoolType), Variable(a), expr(TrueValue))
            )),
          (newVariable: URIO[VariableProvider, TVariable]) => (
            for {
              a <- newVariable
            } yield dummyProof -> Implies(
              pred(EqualTo, expr(BoolType), Variable(a), expr(FalseValue)),
              !pred(EqualTo, expr(BoolType), Variable(a), expr(TrueValue))
            )),

          // Bool == <-> =
          (newVariable: URIO[VariableProvider, TVariable]) => (
            for {
              n <- newVariable
              m <- newVariable
            } yield dummyProof -> Implies(
              pred(EqualTo, expr(BoolType), expr(Eq, Variable(n), Variable(m)), expr(TrueValue)),
              pred(EqualTo, expr(BoolType), Variable(n), Variable(m))
            )
          ),
          (newVariable: URIO[VariableProvider, TVariable]) => (
            for {
              n <- newVariable
              m <- newVariable
            } yield dummyProof -> Implies(
              pred(EqualTo, expr(BoolType), expr(Eq, Variable(n), Variable(m)), expr(FalseValue)),
              !pred(EqualTo, expr(BoolType), Variable(n), Variable(m))
            )
          ),
          (newVariable: URIO[VariableProvider, TVariable]) => (
            for {
              n <- newVariable
              m <- newVariable
            } yield dummyProof -> Implies(
              pred(EqualTo, expr(BoolType), Variable(n), Variable(m)),
              pred(EqualTo, expr(BoolType), expr(Eq, Variable(n), Variable(m)), expr(TrueValue))
            )
          ),
          (newVariable: URIO[VariableProvider, TVariable]) => (
            for {
              n <- newVariable
              m <- newVariable
            } yield dummyProof -> Implies(
              !pred(EqualTo, expr(BoolType), Variable(n), Variable(m)),
              pred(EqualTo, expr(BoolType), expr(Eq, Variable(n), Variable(m)), expr(FalseValue))
            )
          ),

          // a == true or a == false
          (newVariable: URIO[VariableProvider, TVariable]) => (
            for {
              n <- newVariable
            } yield dummyProof -> Or(
              pred(EqualTo, expr(BoolType), Variable(n), expr(TrueValue)),
              pred(EqualTo, expr(BoolType), Variable(n), expr(FalseValue)),
            )
          ),

          // Int == <-> =
          (newVariable: URIO[VariableProvider, TVariable]) => (
            for {
              n <- newVariable
              m <- newVariable
            } yield dummyProof -> Implies(
              pred(EqualTo, expr(BoolType), expr(Eq, Variable(n), Variable(m)), expr(TrueValue)),
              pred(EqualTo, expr(IntType), Variable(n), Variable(m))
            )
          ),
          (newVariable: URIO[VariableProvider, TVariable]) => (
            for {
              n <- newVariable
              m <- newVariable
            } yield dummyProof -> Implies(
              pred(EqualTo, expr(BoolType), expr(Eq, Variable(n), Variable(m)), expr(FalseValue)),
              !pred(EqualTo, expr(IntType), Variable(n), Variable(m))
            )
          ),
          (newVariable: URIO[VariableProvider, TVariable]) => (
            for {
              n <- newVariable
              m <- newVariable
            } yield dummyProof -> Implies(
              pred(EqualTo, expr(IntType), Variable(n), Variable(m)),
              pred(EqualTo, expr(BoolType), expr(Eq, Variable(n), Variable(m)), expr(TrueValue))
            )
          ),
          (newVariable: URIO[VariableProvider, TVariable]) => (
            for {
              n <- newVariable
              m <- newVariable
            } yield dummyProof -> Implies(
              !pred(EqualTo, expr(IntType), Variable(n), Variable(m)),
              pred(EqualTo, expr(BoolType), expr(Eq, Variable(n), Variable(m)), expr(FalseValue))
            )
          ),

          // n == m -> (n <= m) == true
          (newVariable: URIO[VariableProvider, TVariable]) => (
            for {
              n <- newVariable
              m <- newVariable
            } yield dummyProof -> Implies(
              pred(EqualTo, expr(IntType), Variable(n), Variable(m)),
              pred(EqualTo, expr(BoolType), expr(Le, Variable(n), Variable(m)), expr(TrueValue))
            )
          ),

          // (a <= b) == true -> (a == b) \/ ((a < b) == true)
          (newVariable: URIO[VariableProvider, TVariable]) => (
            for {
              n <- newVariable
              m <- newVariable
            } yield dummyProof -> Implies(
              pred(EqualTo, expr(BoolType), expr(Le, Variable(n), Variable(m)), expr(TrueValue)),
              pred(EqualTo, expr(IntType), Variable(n), Variable(m)) |
                pred(EqualTo, expr(BoolType), expr(Lt, Variable(n), Variable(m)), expr(TrueValue))
            )
          ),

          // a <= b or b <= a
          (newVariable: URIO[VariableProvider, TVariable]) => (
            for {
              a <- newVariable
              b <- newVariable
            } yield dummyProof -> (
              pred(EqualTo, expr(BoolType), expr(Le, Variable(a), Variable(b)), expr(TrueValue)) |
                pred(EqualTo, expr(BoolType), expr(Le, Variable(b), Variable(a)), expr(TrueValue))
            )
          ),
        )


    def assertProves(p: Predicate): ZIO[VariableProvider, Any, TestResult] =
      assertZIO(check(p, fuel))(proven)

    def assertNotProves(p: Predicate): ZIO[VariableProvider, Any, TestResult] =
      assertZIO(check(p, fuel))(notProven)

  }

  private def smtContext(extraAssertions: Predicate*): TestContext = new TestContext(extraAssertions)

  override def spec: Spec[Environment & Scope, Any] =
    suite("Proofs")(
      test("prove x == x") {
        smtContext().assertProves(pred(EqualTo, expr(IntType), Variable("x"), Variable("x")))
      },
      test("prove not x == y given (x = y) == false") {
        smtContext(
          pred(EqualTo, expr(BoolType), expr(Eq, Variable("x"), Variable("y")), expr(FalseValue))
        ).assertProves(!pred(EqualTo, expr(IntType), Variable("x"), Variable("y")))
      },
      test("prove not x == y given not y == x") {
        smtContext(
          !pred(EqualTo, expr(IntType), Variable("y"), Variable("x")),
        ).assertProves(!pred(EqualTo, expr(IntType), Variable("x"), Variable("y")))
      },
      test("prove not (x <= y) == true given not x == y, (x < y) == false") {
        smtContext(
          !pred(EqualTo, expr(IntType), Variable("x"), Variable("y")),
          pred(EqualTo, expr(BoolType), expr(Lt, Variable("x"), Variable("y")), expr(FalseValue)),
        ).assertProves(!pred(EqualTo, expr(BoolType), expr(Le, Variable("x"), Variable("y")), expr(TrueValue)))
      },
      test("prove (y <= x) == true given not (x <= y) == true") {
        smtContext(
          !pred(EqualTo, expr(BoolType), expr(Le, Variable("x"), Variable("y")), expr(TrueValue)),
        ).assertProves(pred(EqualTo, expr(BoolType), expr(Le, Variable("y"), Variable("x")), expr(TrueValue)))
      },
    ).provideSome[Environment](VariableProvider.live)

}
