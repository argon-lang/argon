package dev.argon.expr

import dev.argon.util.{*, given}
import dev.argon.prover.{Proof, ProverContext, ProverSyntax}
import dev.argon.prover.prolog.PrologContext
import dev.argon.prover.smt.SmtContext
import dev.argon.util.UniqueIdentifier
import zio.*
import zio.stream.*
import zio.interop.catz.core.*
import dev.argon.ast.IdentifierExpr

import scala.reflect.TypeTest
import cats.data.OptionT
import zio.stm.TMap

abstract class ImplicitResolver[R, E](using TypeTest[Any, E]) {

  val exprContext: ExprContext
  import exprContext.*

  def createHole: ZIO[R, E, Hole]

  sealed trait SubClassResult


  private def substituteVariables(vars: Map[Var, Expr])(expr: Expr): Expr =
    Substitution.substitute(exprContext)(vars)(expr)

  protected def evaluator(model: Ref[Model]): Evaluator[R, E] { val exprContext: ImplicitResolver.this.exprContext.type }

  object ExprProverSyntax extends ProverSyntax {
    override type TVariable = Hole
    override type TPredicateExpr = Expr

    override def variableToExpr(v: Hole): Expr =
      Expr.Hole(v)
  }

  enum TCAtomicProof {
    case ExprProof(expr: Expr)
  }

  enum ExprRelation derives CanEqual {
    case SubType, SuperType
    case TypeEquality
    case SyntacticEquality
  }

  final case class Assertion(witness: Expr, assertionType: Expr)


  trait AssertionBuilder {
    def create(newVariable: ZIO[R, E, Hole]): ZIO[R, E, Assertion]
  }

  trait IRProverContext
  (
    givenAssertions: Seq[AssertionBuilder],
    knownVarValues: Map[Var, Expr],
    initialFuel: Fuel,
  ) extends ProverContext[R, E] {
    override val syntax: ExprProverSyntax.type = ExprProverSyntax
    import syntax.*

    override type ProofAtom = TCAtomicProof

    override type Model = exprContext.Model


    protected override def freshAssertions(model: Model): Seq[ZIO[R, E, Hole] => ZIO[R, E, (Proof[ProofAtom], syntax.Predicate)]] =
      builtinAssertions ++ givenAssertions.map { createAssertion =>
        (newVariable: ZIO[R, E, Hole]) =>
          for
            assertion <- createAssertion.create(newVariable)
            assertionType <- exprToGoal(assertion.assertionType, model, initialFuel)
          yield (Proof.Atomic(TCAtomicProof.ExprProof(assertion.witness)), assertionType)
      }

    protected override def unifyPredicateExpression(f1: Expr, f2: Expr, model: Ref[Model], fuel: Fuel): ZIO[R, E, Boolean] =
      Unification.unify(exprContext)(model, evaluator(model), fuel)(f1, f2)

    protected def builtinAssertions: Seq[ZIO[R, E, Hole] => ZIO[R, E, (Proof[ProofAtom], Predicate)]] =
      Seq(
        // ------
        // A == A
        newVariable => (for {
          t <- newVariable
          a <- newVariable
        } yield (
          Proof.Atomic(TCAtomicProof.ExprProof(
            Expr.Builtin(Builtin.EqualToRefl(
              Expr.Hole(t),
              Expr.Hole(a),
            ))
          )) -> PredicateExpression(
            Expr.Builtin(Builtin.EqualTo(
              Expr.Hole(t),
              Expr.Hole(a),
              Expr.Hole(a),
            ))
          ))
        ),
      )

    def exprToGoal(expr: Expr, model: Model, fuel: Fuel): ZIO[R, E, Predicate] =
      for
        modelRef <- Ref.make(model)
        normExpr <- evaluator(modelRef).normalizeToValue(expr, fuel)
        model <- modelRef.get
        res <- normExpr match {
          case Expr.Builtin(Builtin.Binary(BinaryBuiltin.ConjunctionType, a, b)) =>
            for
              a2 <- exprToGoal(a, model, fuel.consume)
              b2 <- exprToGoal(b, model, fuel.consume)
            yield And(a2, b2)

          case Expr.Builtin(Builtin.Binary(BinaryBuiltin.DisjunctionType, a, b)) =>
            for
              a2 <- exprToGoal(a, model, fuel.consume)
              b2 <- exprToGoal(b, model, fuel.consume)
            yield Or(a2, b2)

          case Expr.Builtin(Builtin.Nullary(NullaryBuiltin.NeverType)) =>
            ZIO.succeed(PropFalse)

          case Expr.Builtin(Builtin.EqualTo(t, a, b)) =>
            evaluator(modelRef).normalizeToValue(t, fuel).flatMap {
              case Expr.Builtin(Builtin.Nullary(NullaryBuiltin.BoolType)) =>
                for
                  a2 <- exprToGoal(a, model, fuel.consume)
                  b2 <- exprToGoal(b, model, fuel.consume)
                yield And(Implies(a2, b2), Implies(b2, a2))

              case _ => ZIO.succeed(PredicateExpression(normExpr))
            }

          case Expr.Builtin(Builtin.Unary(UnaryBuiltin.BoolNot, a)) =>
            for
              a2 <- exprToGoal(a, model, fuel.consume)
            yield Implies(a2, PropFalse)

          case Expr.Builtin(Builtin.Binary(BinaryBuiltin.BoolEQ, a, b)) =>
            for
              a2 <- exprToGoal(a, model, fuel.consume)
              b2 <- exprToGoal(b, model, fuel.consume)
            yield And(Implies(a2, b2), Implies(b2, a2))

          case Expr.Builtin(Builtin.Binary(BinaryBuiltin.BoolNE, a, b)) =>
            for
              a2 <- exprToGoal(a, model, fuel.consume)
              b2 <- exprToGoal(b, model, fuel.consume)
            yield And(Implies(a2, Implies(b2, PropFalse)), Implies(Implies(a2, PropFalse), b2))

          case Expr.BoolLiteral(true) => ZIO.succeed(PropTrue)
          case Expr.BoolLiteral(false) => ZIO.succeed(PropFalse)

          case Expr.FunctionType(a, b) =>
            for {
              a2 <- exprToGoal(a.varType, model, fuel.consume)
              b2 <- exprToGoal(b, model, fuel.consume)
            } yield Implies(a2, b2)

          case e =>
            ZIO.succeed(PredicateExpression(e))
        }
      yield res

    def proofAtomicAsExpr(proof: Proof[TCAtomicProof]): ZIO[R, E, Proof[Expr]] =
      proof match
        case Proof.Atomic(TCAtomicProof.ExprProof(expr)) =>
          ZIO.succeed(Proof.Atomic(expr))

        case Proof.TrueIsTrue =>
          ZIO.succeed(Proof.TrueIsTrue)

        case Proof.Identifier(id) => ZIO.succeed(Proof.Identifier(id))
        case Proof.ImplicaitonAbstraction(id, body) =>
          for
            body <- proofAtomicAsExpr(body)
          yield Proof.ImplicaitonAbstraction(id, body)

        case Proof.ModusPonens(implication, premise) =>
          for
            implication <- proofAtomicAsExpr(implication)
            premise <- proofAtomicAsExpr(premise)
          yield Proof.ModusPonens(implication, premise)

        case Proof.ModusTollens(implication, consequentFalse) =>
          for
            implication <- proofAtomicAsExpr(implication)
            consequentFalse <- proofAtomicAsExpr(consequentFalse)
          yield Proof.ModusTollens(implication, consequentFalse)


        case Proof.ConjunctIntro(a, b) =>
          for
            a <- proofAtomicAsExpr(a)
            b <- proofAtomicAsExpr(b)
          yield Proof.ConjunctIntro(a, b)

        case Proof.DisjunctIntroLeft(p) =>
          for
            p <- proofAtomicAsExpr(p)
          yield Proof.DisjunctIntroLeft(p)

        case Proof.DisjunctIntroRight(p) =>
          for
            p <- proofAtomicAsExpr(p)
          yield Proof.DisjunctIntroLeft(p)

        case Proof.DisjunctCommute(p) =>
          for
            p <- proofAtomicAsExpr(p)
          yield Proof.DisjunctCommute(p)

        case Proof.ConjunctCommute(p) =>
          for
            p <- proofAtomicAsExpr(p)
          yield Proof.ConjunctCommute(p)

        case Proof.DeMorganAndPullNotOut(p) =>
          for
            p <- proofAtomicAsExpr(p)
          yield Proof.DeMorganAndPullNotOut(p)

        case Proof.DeMorganOrPullNotOut(p) =>
          for
            p <- proofAtomicAsExpr(p)
          yield Proof.DeMorganOrPullNotOut(p)

        case Proof.DeMorganAndPushNotIn(p) =>
          for
            p <- proofAtomicAsExpr(p)
          yield Proof.DeMorganAndPushNotIn(p)

        case Proof.DeMorganOrPushNotIn(p) =>
          for
            p <- proofAtomicAsExpr(p)
          yield Proof.DeMorganOrPushNotIn(p)

        case Proof.DoubleNegIntro(p) =>
          for
            p <- proofAtomicAsExpr(p)
          yield Proof.DoubleNegIntro(p)

        case Proof.HypotheticalSyllogism(pImpliesQ, qImpliesR) =>
          for
            pImpliesQ <- proofAtomicAsExpr(pImpliesQ)
            qImpliesR <- proofAtomicAsExpr(qImpliesR)
          yield Proof.HypotheticalSyllogism(pImpliesQ, qImpliesR)

      end match
  }

  protected sealed class IRPrologContext
  (
    createdHoles: Ref[Set[Hole]],
    givenAssertions: Seq[AssertionBuilder],
    knownVarValues: Map[Var, Expr],
    initialFuel: Fuel,
  ) extends PrologContext[R, E] with IRProverContext(givenAssertions, knownVarValues, initialFuel) {
    import syntax.*


    protected override def newVariable: ZIO[R, E, ExprProverSyntax.TVariable] =
      createHole.tap { hole =>
        createdHoles.update(_ + hole)
      }

    protected override def intrinsicPredicate
      (predicate: Expr, model: Model, solveState: SolveState)
      : ZStream[R, E, ProofResult.Definitive] =
      ZStream.empty
      

  }

  protected sealed class IRSmtContext
  (
    givenAssertions: Seq[AssertionBuilder],
    knownVarValues: Map[Var, Expr],
    initialFuel: Fuel,
  ) extends SmtContext[R, E] with IRProverContext(givenAssertions, knownVarValues, initialFuel) {
    import syntax.*

    override protected def newVariable: ZIO[R, E, exprContext.Hole] = createHole

    override protected def predicateReferencesVariable(p: exprContext.Expr, v: exprContext.Hole): Boolean =
      HoleScanner.hasHole(exprContext)(v)(p)

    override protected def assumeResultProof: Proof[TCAtomicProof] =
      Proof.Atomic(TCAtomicProof.ExprProof(Expr.ErasedValue()))

    override protected def normalizePredicateExpression(p: Expr, model: Model, fuel: Fuel): ZIO[R, E, Expr] =
      Ref.make(model).flatMap { model =>
        evaluator(model).normalizeToValue(p, fuel)
      }

    override protected def matchPredicateExpr(pf: TPredicateExpr, quantPF: TPredicateExpr, state: ProverState, quantVars: Set[TVariable]): ZIO[R, E, Option[Map[TVariable, TPredicateExpr]]] =
      Ref.make(state.model)
        .flatMap { model =>
          Unification.unify[R, E](exprContext)(model, evaluator(model), initialFuel)(pf, quantPF)
            .flatMap {
              case true =>
                model.get.map { model =>
                  val varMap = quantVars.view
                    .map { v => v -> model.resolveHole(v).getOrElse(Expr.Hole(v)) }
                    .toMap

                  Some(varMap)
                }

              case false => ZIO.none
            }
        }

    override protected val theories: Seq[Theory] = Seq(
      new Theory {
        override def check(assigned: Seq[KnownPredicate], unassigned: Seq[Expr], model: Model, fuel: Fuel): ZIO[R, E | SmtConstraint, Seq[KnownPredicate]] =
          ZIO.foreachDiscard(assigned) {
            case KnownPredicate(p @ Expr.Builtin(Builtin.EqualTo(_, a, b)), false) =>
              for
                model <- Ref.make(model)
                a: Expr <- evaluator(model).normalizeToValue(a, fuel)
                b: Expr <- evaluator(model).normalizeToValue(b, fuel)
                _ <- ZIO.fail(SmtConstraint(PredicateExpression(p))).whenDiscard(a == b)
              yield ()

            case _ => ZIO.unit
          } *>
            ZIO.foreach(unassigned) {
              case p @ Expr.Builtin(Builtin.EqualTo(_, a, b)) =>
                for
                  model <- Ref.make(model)
                  a: Expr <- evaluator(model).normalizeToValue(a, fuel)
                  b: Expr <- evaluator(model).normalizeToValue(b, fuel)
                yield (if a == b then Some(KnownPredicate(p, true)) else None)

              case _ => ZIO.none
            }.map(_.flatten)
      },
    )

    private def isEqualExprs(a: Expr, b: Expr): Boolean =
      a == b || ((a, b) match {
        case _ => false
      })

    override protected def substituteVariablesPE(varMap: Map[Hole, Expr])(pf: Expr): Expr =
      HoleSubstitution.substitute(exprContext)(varMap)(pf)


    protected override def freshAssertions(model: Model): Seq[ZIO[R, E, exprContext.Hole] => ZIO[R, E, (Proof[TCAtomicProof], Predicate)]] =
      super.freshAssertions(model) ++ Seq(
        // A == B
        // ------
        // B == A
        newVariable => (for {
          t <- newVariable
          a <- newVariable
          b <- newVariable
        } yield (
          Proof.Atomic(TCAtomicProof.ExprProof(Expr.ErasedValue())) -> Implies(
            PredicateExpression(Expr.Builtin(Builtin.EqualTo(Expr.Hole(t), Expr.Hole(a), Expr.Hole(b)))),
            PredicateExpression(Expr.Builtin(Builtin.EqualTo(Expr.Hole(t), Expr.Hole(b), Expr.Hole(a)))),
          ))
        ),


        // A, B: string
        // ------------------
        // A = B == !(A != B)
        newVariable => (for {
          a <- newVariable
          b <- newVariable
        } yield (
          Proof.Atomic(TCAtomicProof.ExprProof(Expr.ErasedValue())) ->
            PredicateExpression(Expr.Builtin(Builtin.EqualTo(
              boolType,
              binOp(BinaryBuiltin.StringEQ, Expr.Hole(a), Expr.Hole(b)),
              unOp(UnaryBuiltin.BoolNot, binOp(BinaryBuiltin.StringNE, Expr.Hole(a), Expr.Hole(b))),
            )))
        )),

        // A, B: string
        // ------------------
        // !(A = B) == (A != B)
        newVariable => (for {
          a <- newVariable
          b <- newVariable
        } yield (
          Proof.Atomic(TCAtomicProof.ExprProof(Expr.ErasedValue())) ->
            PredicateExpression(Expr.Builtin(Builtin.EqualTo(
              boolType,
              unOp(UnaryBuiltin.BoolNot, binOp(BinaryBuiltin.StringEQ, Expr.Hole(a), Expr.Hole(b))),
              binOp(BinaryBuiltin.StringNE, Expr.Hole(a), Expr.Hole(b)),
            )))
          )
        ),

        // A, B: string
        // ------------------
        // (A != B) == !(A = B)
        newVariable => (for {
          a <- newVariable
          b <- newVariable
        } yield (
          Proof.Atomic(TCAtomicProof.ExprProof(Expr.ErasedValue())) ->
            Implies(
              PredicateExpression(binOp(BinaryBuiltin.StringNE, Expr.Hole(a), Expr.Hole(b))),
              Implies(PredicateExpression(binOp(BinaryBuiltin.StringEQ, Expr.Hole(a), Expr.Hole(b))), PropFalse),
            )
        )),
        newVariable => (for {
          a <- newVariable
          b <- newVariable
        } yield (
          Proof.Atomic(TCAtomicProof.ExprProof(Expr.ErasedValue())) ->
            Implies(
              Implies(PredicateExpression(binOp(BinaryBuiltin.StringEQ, Expr.Hole(a), Expr.Hole(b))), PropFalse),
                PredicateExpression(binOp(BinaryBuiltin.StringNE, Expr.Hole(a), Expr.Hole(b))),
            )
        )),

        // A, B: string
        // ------------------
        // (A == B) == (A = B)
        newVariable => (for {
          a <- newVariable
          b <- newVariable
        } yield (
          Proof.Atomic(TCAtomicProof.ExprProof(Expr.ErasedValue())) ->
            Implies(
              PredicateExpression(binOp(BinaryBuiltin.StringEQ, Expr.Hole(a), Expr.Hole(b))),
              PredicateExpression(Expr.Builtin(Builtin.EqualTo(
                Expr.Builtin(Builtin.Nullary(NullaryBuiltin.StringType)),
                Expr.Hole(a),
                Expr.Hole(b),
              ))),
            )
          )),
        newVariable => (for {
          a <- newVariable
          b <- newVariable
        } yield (
          Proof.Atomic(TCAtomicProof.ExprProof(Expr.ErasedValue())) ->
            Implies(
              PredicateExpression(Expr.Builtin(Builtin.EqualTo(
                Expr.Builtin(Builtin.Nullary(NullaryBuiltin.StringType)),
                Expr.Hole(a),
                Expr.Hole(b),
              ))),
              PredicateExpression(binOp(BinaryBuiltin.StringEQ, Expr.Hole(a), Expr.Hole(b))),
            )
          )),

        // A, B: int
        // ------------------
        // (A != B) == !(A = B)
        newVariable => (for {
          a <- newVariable
          b <- newVariable
        } yield (
          Proof.Atomic(TCAtomicProof.ExprProof(Expr.ErasedValue())) ->
            Implies(
              PredicateExpression(binOp(BinaryBuiltin.IntNE, Expr.Hole(a), Expr.Hole(b))),
              Implies(PredicateExpression(binOp(BinaryBuiltin.IntEQ, Expr.Hole(a), Expr.Hole(b))), PropFalse),
            )
        )),
        newVariable => (for {
          a <- newVariable
          b <- newVariable
        } yield (
          Proof.Atomic(TCAtomicProof.ExprProof(Expr.ErasedValue())) ->
            Implies(
              Implies(PredicateExpression(binOp(BinaryBuiltin.IntEQ, Expr.Hole(a), Expr.Hole(b))), PropFalse),
              PredicateExpression(binOp(BinaryBuiltin.IntNE, Expr.Hole(a), Expr.Hole(b))),
            )
        )),

        // A, B: int
        // ------------------
        // (A == B) == (A = B)
        newVariable => (for {
          a <- newVariable
          b <- newVariable
        } yield (
          Proof.Atomic(TCAtomicProof.ExprProof(Expr.ErasedValue())) ->
            Implies(
              PredicateExpression(binOp(BinaryBuiltin.IntEQ, Expr.Hole(a), Expr.Hole(b))),
              PredicateExpression(Expr.Builtin(Builtin.EqualTo(
                Expr.Builtin(Builtin.Nullary(NullaryBuiltin.IntType)),
                Expr.Hole(a),
                Expr.Hole(b),
              ))),
            )
        )),
        newVariable => (for {
          a <- newVariable
          b <- newVariable
        } yield (
          Proof.Atomic(TCAtomicProof.ExprProof(Expr.ErasedValue())) ->
            Implies(
              PredicateExpression(Expr.Builtin(Builtin.EqualTo(
                Expr.Builtin(Builtin.Nullary(NullaryBuiltin.IntType)),
                Expr.Hole(a),
                Expr.Hole(b),
              ))),
              PredicateExpression(binOp(BinaryBuiltin.IntEQ, Expr.Hole(a), Expr.Hole(b))),
            )
        )),
        
        // A, B: int
        // A < B or A = B or A > B
        newVariable => (for {
          a <- newVariable
          b <- newVariable
        } yield (
          Proof.Atomic(TCAtomicProof.ExprProof(Expr.ErasedValue())) ->
            Or(
              PredicateExpression(binOp(BinaryBuiltin.IntLT, Expr.Hole(a), Expr.Hole(b))),
              Or(
                PredicateExpression(binOp(BinaryBuiltin.IntEQ, Expr.Hole(a), Expr.Hole(b))),
                PredicateExpression(binOp(BinaryBuiltin.IntGT, Expr.Hole(a), Expr.Hole(b))),
              ),
            )
        )),
      )
  }

  private def binOp(op: BinaryBuiltin, a: Expr, b: Expr): Expr =
    Expr.Builtin(Builtin.Binary(op, a, b))

  private def unOp(op: UnaryBuiltin, a: Expr): Expr =
    Expr.Builtin(Builtin.Unary(op, a))

  private def boolType: Expr =
    Expr.Builtin(Builtin.Nullary(NullaryBuiltin.BoolType))

  private def stringType: Expr =
    Expr.Builtin(Builtin.Nullary(NullaryBuiltin.StringType))

  final case class ResolvedImplicit(proof: Proof[Expr], model: Model)

  private def tryResolveWithProver(implicitType: Expr, model: Model, proverContext: IRProverContext, fuel: Fuel): ZIO[R, E, Option[ResolvedImplicit]] =
    for
      goal <- proverContext.exprToGoal(implicitType, model, fuel)

      result <- proverContext.check(goal, model, fuel).flatMap {
        case proverContext.ProofResult.Yes(proof, model) =>
          for
            proof <- proverContext.proofAtomicAsExpr(proof)
          yield Some(ResolvedImplicit(proof, model))

        case proverContext.ProofResult.No(_, _) | _: proverContext.ProofResult.Unknown.type => ZIO.none
      }

    yield result

  final case class FuelSpecifiers
  (
    evaluatorFuel: Fuel,
    prologFuel: Fuel,
    smtFuel: Fuel,
  )

  final def tryResolve(implicitType: Expr, model: Model, givenAssertions: Seq[AssertionBuilder], knownVarValues: Map[Var, Expr], fuel: FuelSpecifiers)
  : ZIO[R, E, Option[ResolvedImplicit]] =
    def prologAttempt =
      val isEqualTo = implicitType match {
        case Expr.Builtin(Builtin.EqualTo(_, _, _)) => true
        case _ => false
      }

      if isEqualTo then
        ZIO.none
      else
        Ref.make(Set.empty[Hole]).flatMap { createdHoles =>
          val proverContext = new IRPrologContext(createdHoles, givenAssertions, knownVarValues, fuel.prologFuel)
          tryResolveWithProver(implicitType, model, proverContext, fuel.prologFuel)
        }
    end prologAttempt


    def smtAttempt = tryResolveWithProver(implicitType, model, new IRSmtContext(givenAssertions, knownVarValues, fuel.smtFuel), fuel.smtFuel)

    prologAttempt.flatMap {
      case Some(res) => ZIO.some(res)
      case None => smtAttempt
    }
  end tryResolve
}
