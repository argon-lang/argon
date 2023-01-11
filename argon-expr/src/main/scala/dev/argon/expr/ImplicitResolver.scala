package dev.argon.expr

import dev.argon.util.{*, given}
import dev.argon.prover.{Proof, ProverSyntax, ProverContext}
import dev.argon.prover.prolog.PrologContext
import dev.argon.prover.smt.SmtContext
import dev.argon.util.UniqueIdentifier
import zio.*
import zio.stream.*

abstract class ImplicitResolver[R, E] {

  val exprContext: ExprContext
  import exprContext.*

  def createHole: ZIO[R, E, THole]

  sealed trait SubClassResult

  protected def isSubClass
  (
    prologContext: IRPrologContext,
    classA: TClass,
    aArgs: Seq[ExprProverSyntax.Expr],
    classB: TClass,
    bArgs: Seq[ExprProverSyntax.Expr],
    model: prologContext.Model,
    solveState: prologContext.SolveState,
  )
    : ZStream[R, Either[E, prologContext.ProofResult.No], prologContext.ProofResult.Yes]

  protected def isSubTrait
  (
    prologContext: IRPrologContext,
    traitA: TTrait,
    aArgs: Seq[ExprProverSyntax.Expr],
    traitB: TTrait,
    bArgs: Seq[ExprProverSyntax.Expr],
    model: prologContext.Model,
    solveState: prologContext.SolveState,
  )
  : ZStream[R, Either[E, prologContext.ProofResult.No], prologContext.ProofResult.Yes]

  protected def classImplementsTrait
  (
    prologContext: IRPrologContext,
    classA: TClass,
    aArgs: Seq[ExprProverSyntax.Expr],
    traitB: TTrait,
    bArgs: Seq[ExprProverSyntax.Expr],
    model: prologContext.Model,
    solveState: prologContext.SolveState,
  )
  : ZStream[R, Either[E, prologContext.ProofResult.No], prologContext.ProofResult.Yes]

  protected def typeOfClass(classObj: TClass, args: Seq[WrapExpr]): ZIO[R, E, WrapExpr]
  protected def typeOfTrait(traitObj: TTrait, args: Seq[WrapExpr]): ZIO[R, E, WrapExpr]

  // These relation methods must match the arguments for the expression constructors
  protected def traitRelations(arTrait: TTrait): ZIO[R, E, Seq[ExprRelation]]
  protected def classRelations(arClass: TClass): ZIO[R, E, Seq[ExprRelation]]
  protected def functionRelations(function: TFunction): ZIO[R, E, Seq[ExprRelation]]
  protected def methodRelations(method: TMethod): ZIO[R, E, Seq[ExprRelation]]
  protected def classConstructorRelations(classCtor: TClassConstructor): ZIO[R, E, Seq[ExprRelation]]

  protected def substituteVariables(vars: Map[TVariable, WrapExpr])(expr: WrapExpr): WrapExpr

  protected def natLessThanFunction: ZIO[R, E, TFunction]
  protected def boolType: ZIO[R, E, WrapExpr]

  protected def invalidExpr: ZIO[R, E, Nothing]
  protected def invalidPredicateExpr: ZIO[R, E, Nothing]

  protected lazy val evaluator: Evaluator[R, E] { val exprContext: ImplicitResolver.this.exprContext.type }

  object ExprProverSyntax extends ProverSyntax {
    override type TVariable = THole
    override type TConstructor = ExprConstructor
    override type TPredicateFunction = ExprConstructor

    override def variableCanEqual: CanEqual[THole, THole] = summon[CanEqual[THole, THole]]
    override def constructorCanEqual: CanEqual[ExprConstructor, ExprConstructor] = summon[CanEqual[ExprConstructor, ExprConstructor]]
    override def predicateFunctionCanEqual: CanEqual[ExprConstructor, ExprConstructor] = summon[CanEqual[ExprConstructor, ExprConstructor]]
  }

  enum TCAtomicProof {
    case ExprProof(expr: WrapExpr)
  }

  enum ExprRelation derives CanEqual {
    case SubType, SuperType
    case TypeEquality
    case SyntacticEquality
  }

  final case class Assertion(witness: WrapExpr, assertionType: WrapExpr)


  trait IRProverContext
  (
    givenAssertions: Seq[ZIO[R, E, THole] => ZIO[R, E, Assertion]],
    knownVarValues: Map[TVariable, WrapExpr],
    initialFuel: Int,
  ) extends ProverContext[R, E] {
    override val syntax: ExprProverSyntax.type = ExprProverSyntax
    import syntax.*

    override type ProofAtom = TCAtomicProof

    override type TRelation = ExprRelation
    override type TConstraints = ExprConstraints[Expr]

    protected override def assertions: Seq[ZIO[R, E, THole] => ZIO[R, E, (Proof[ProofAtom], syntax.Predicate)]] =
      builtinAssertions ++ givenAssertions.map { createAssertion =>
        (newVariable: ZIO[R, E, THole]) =>
          for
            assertion <- createAssertion(newVariable)
            assertionType <- arExprToGoal(assertion.assertionType, initialFuel)
          yield (Proof.Atomic(TCAtomicProof.ExprProof(assertion.witness)), assertionType)
      }

    protected override def normalize(expr: syntax.Value, substitutions: Model, fuel: Int): ZIO[R, E, syntax.Expr] =
      def normalizeNonLocal =
        exprToWrapExpr(expr).flatMap { expr =>
          evaluator.normalizeTopLevelWrap(expr, fuel - 1)
            .map(wrapExprToExpr)
        }

      expr match {
        case syntax.Value(ExprConstructor.LoadVariable(variable), _) =>
          knownVarValues.get(variable) match {
            case Some(value) => normalizeExpr(wrapExprToExpr(value), substitutions, fuel - 1)
            case None => normalizeNonLocal
          }

        case _ => normalizeNonLocal
      }
    end normalize

    protected override def otherForEquivalenceRelation(constraints: ExprConstraints[syntax.Expr]): Option[syntax.Expr] =
      constraints match {
        case ExprEqualConstraint(other) => Some(other)
        case ExprTypeBounds(_, _) => None
      }

    protected def builtinAssertions: Seq[ZIO[R, E, TVariable] => ZIO[R, E, (Proof[ProofAtom], Predicate)]] =
      Seq(
        // ------
        // A == A
        newVariable => (for {
          t <- newVariable
          a <- newVariable
        } yield (
          Proof.Atomic(TCAtomicProof.ExprProof(WrapExpr.OfExpr(ArExpr(
            ExprConstructor.AssumeErasedValue,
            EmptyTuple,
          )))) -> PredicateFunction(ExprConstructor.EqualTo, Seq(Variable(t), Variable(a), Variable(a)))
          )),

        // ------
        // A <: A
        newVariable => (for {
          a <- newVariable
        } yield (
          Proof.Atomic(TCAtomicProof.ExprProof(wrapExpr(
            ExprConstructor.AssumeErasedValue,
            EmptyTuple,
          ))) -> PredicateFunction(ExprConstructor.SubtypeWitnessType, Seq(Variable(a), Variable(a)))
          )),

        // A <: B1 and A <: B2
        // -------------------
        // A <: (B1 & B2)
        newVariable => (for {
          a <- newVariable
          b1 <- newVariable
          b2 <- newVariable
        } yield (
          Proof.Atomic(TCAtomicProof.ExprProof(wrapExpr(
            ExprConstructor.AssumeErasedValue,
            EmptyTuple,
          ))) -> Implies(
            And(
              PredicateFunction(ExprConstructor.SubtypeWitnessType, Seq(Variable(a), Variable(b1))),
              PredicateFunction(ExprConstructor.SubtypeWitnessType, Seq(Variable(a), Variable(b2))),
            ),
            PredicateFunction(
              ExprConstructor.SubtypeWitnessType,
              Seq(Variable(a), Value(ExprConstructor.IntersectionType, Seq(Variable(b1), Variable(b2)))),
            ),
          )
          )),

        // A <: B1 or A <: B2
        // ------------------
        // A <: (B1 | B2)
        newVariable => (for {
          a <- newVariable
          b1 <- newVariable
          b2 <- newVariable
        } yield (
          Proof.Atomic(TCAtomicProof.ExprProof(wrapExpr(
            ExprConstructor.AssumeErasedValue,
            EmptyTuple,
          ))) -> Implies(
            Or(
              PredicateFunction(ExprConstructor.SubtypeWitnessType, Seq(Variable(a), Variable(b1))),
              PredicateFunction(ExprConstructor.SubtypeWitnessType, Seq(Variable(a), Variable(b2))),
            ),
            PredicateFunction(
              ExprConstructor.SubtypeWitnessType,
              Seq(Variable(a), Value(ExprConstructor.UnionType, Seq(Variable(b1), Variable(b2)))),
            ),
          )
          )),

        // A1 <: B or A2 <: B
        // ------------------
        // A1 & A2 <: B
        newVariable => (for {
          a1 <- newVariable
          a2 <- newVariable
          b <- newVariable
        } yield (
          Proof.Atomic(TCAtomicProof.ExprProof(wrapExpr(
            ExprConstructor.AssumeErasedValue,
            EmptyTuple,
          ))) -> Implies(
            Or(
              PredicateFunction(ExprConstructor.SubtypeWitnessType, Seq(Variable(a1), Variable(b))),
              PredicateFunction(ExprConstructor.SubtypeWitnessType, Seq(Variable(a2), Variable(b))),
            ),
            PredicateFunction(
              ExprConstructor.SubtypeWitnessType,
              Seq(Value(ExprConstructor.IntersectionType, Seq(Variable(a1), Variable(a2))), Variable(b)),
            ),
          )
          )),

        // A1 <: B and A2 <: B
        // -------------------
        // (A1 | A2) <: B
        newVariable => (for {
          a1 <- newVariable
          a2 <- newVariable
          b <- newVariable
        } yield (
          Proof.Atomic(TCAtomicProof.ExprProof(wrapExpr(
            ExprConstructor.AssumeErasedValue,
            EmptyTuple,
          ))) -> Implies(
            And(
              PredicateFunction(ExprConstructor.SubtypeWitnessType, Seq(Variable(a1), Variable(b))),
              PredicateFunction(ExprConstructor.SubtypeWitnessType, Seq(Variable(a2), Variable(b))),
            ),
            PredicateFunction(
              ExprConstructor.SubtypeWitnessType,
              Seq(Value(ExprConstructor.UnionType, Seq(Variable(a1), Variable(a2))), Variable(b)),
            ),
          )
          )),

        // A <: B and C <: D
        // --------------------
        // (B -> C) <: (A -> D)
        newVariable => (for {
          a <- newVariable
          b <- newVariable
          c <- newVariable
          d <- newVariable
        } yield (
          Proof.Atomic(TCAtomicProof.ExprProof(wrapExpr(
            ExprConstructor.AssumeErasedValue,
            EmptyTuple,
          ))) -> Implies(
            And(
              PredicateFunction(ExprConstructor.SubtypeWitnessType, Seq(Variable(a), Variable(b))),
              PredicateFunction(ExprConstructor.SubtypeWitnessType, Seq(Variable(c), Variable(d))),
            ),
            PredicateFunction(
              ExprConstructor.SubtypeWitnessType,
              Seq(
                Value(ExprConstructor.FunctionType, Vector(Variable(b), Variable(c))),
                Value(ExprConstructor.FunctionType, Vector(Variable(a), Variable(d))),
              ),
            ),
          )
          )),

        // (B < A) == true
        // ------------------
        // TypeN B <: TypeN A
        newVariable => (for {
          bType <- boolType
          lt <- natLessThanFunction
          a <- newVariable
          b <- newVariable
        } yield (
          Proof.Atomic(TCAtomicProof.ExprProof(wrapExpr(
            ExprConstructor.AssumeErasedValue,
            EmptyTuple,
          ))) -> Implies(
            PredicateFunction(
              ExprConstructor.EqualTo,
              Seq(
                wrapExprToExpr(bType),
                Value(ExprConstructor.FunctionCall(lt), Seq(Variable(b), Variable(a))),
                Value(ExprConstructor.LoadConstantBool(true), Seq()),
              ),
            ),
            PredicateFunction(
              ExprConstructor.SubtypeWitnessType,
              Seq(
                Value(ExprConstructor.TypeN, Seq(Variable(b))),
                Value(ExprConstructor.TypeN, Seq(Variable(a))),
              ),
            ),
          )
          )),
      )



    def exprToWrapExpr(expr: Expr): ZIO[R, E, WrapExpr] =
      expr match {
        case Value(ctor, args) =>
          ZIO.foreach(args)(exprToWrapExpr).flatMap { argExprs =>
            ctor.argsFromExprs(argExprs) match {
              case Some(ctorArgs) => ZIO.succeed(WrapExpr.OfExpr(ArExpr(ctor, ctorArgs)))
              case None =>
                ZIO.logError(s"Could not from prover expression back to argon: $ctor, $args") *>
                  invalidExpr
            }
          }

        case Variable(variable) => ZIO.succeed(WrapExpr.OfHole(variable))
      }

    def exprToWrapExprError(expr: Expr): ZIO[R, Error, WrapExpr] = exprToWrapExpr(expr).mapError(Left.apply)

    def wrapExprToExpr(e: WrapExpr): Expr =
      e match {
        case WrapExpr.OfExpr(expr) =>
          Value(expr.constructor, expr.constructor.argsToExprs(expr.args).map(wrapExprToExpr))
        case WrapExpr.OfHole(variable) => Variable(variable)
      }

    def arExprToGoal(expr: WrapExpr, fuel: Int): ZIO[R, E, Predicate] =
      evaluator.normalizeTopLevelWrap(expr, fuel).flatMap {
        case WrapExpr.OfExpr(expr) =>
          expr.constructor match {
            case ExprConstructor.ConjunctionType =>
              val (a, b) = expr.args.asInstanceOf[ExprConstructor.ConjunctionTypeArgs]
              for {
                a2 <- arExprToGoal(a, fuel - 1)
                b2 <- arExprToGoal(b, fuel - 1)
              } yield And(a2, b2)

            case ExprConstructor.DisjunctionType =>
              val (a, b) = expr.args.asInstanceOf[ExprConstructor.DisjunctionTypeArgs]
              for {
                a2 <- arExprToGoal(a, fuel - 1)
                b2 <- arExprToGoal(b, fuel - 1)
              } yield Or(a2, b2)

            case ExprConstructor.FunctionType =>
              val (a, b) = expr.args.asInstanceOf[ExprConstructor.ConjunctionTypeArgs]
              for {
                a2 <- arExprToGoal(a, fuel - 1)
                b2 <- arExprToGoal(b, fuel - 1)
              } yield Implies(a2, b2)

            case ExprConstructor.NeverType => ZIO.succeed(PropFalse)

            case _ =>
              val args = expr.constructor.argsToExprs(expr.args).map(wrapExprToExpr)
              ZIO.succeed(PredicateFunction(expr.constructor, args))
          }

        case WrapExpr.OfHole(_) => invalidPredicateExpr
      }

    def proofAtomicAsWrapExpr(proof: Proof[TCAtomicProof]): ZIO[R, E, Proof[WrapExpr]] =
      proof match
        case Proof.Atomic(TCAtomicProof.ExprProof(expr)) =>
          ZIO.succeed(Proof.Atomic(expr))

        case Proof.Identifier(id) => ZIO.succeed(Proof.Identifier(id))
        case Proof.ImplicaitonAbstraction(id, body) =>
          for
            body <- proofAtomicAsWrapExpr(body)
          yield Proof.ImplicaitonAbstraction(id, body)

        case Proof.ModusPonens(implication, premise) =>
          for
            implication <- proofAtomicAsWrapExpr(implication)
            premise <- proofAtomicAsWrapExpr(premise)
          yield Proof.ModusPonens(implication, premise)

        case Proof.ModusTollens(implication, consequentFalse) =>
          for
            implication <- proofAtomicAsWrapExpr(implication)
            consequentFalse <- proofAtomicAsWrapExpr(consequentFalse)
          yield Proof.ModusTollens(implication, consequentFalse)


        case Proof.ConjunctIntro(a, b) =>
          for
            a <- proofAtomicAsWrapExpr(a)
            b <- proofAtomicAsWrapExpr(b)
          yield Proof.ConjunctIntro(a, b)

        case Proof.DisjunctIntroLeft(p) =>
          for
            p <- proofAtomicAsWrapExpr(p)
          yield Proof.DisjunctIntroLeft(p)

        case Proof.DisjunctIntroRight(p) =>
          for
            p <- proofAtomicAsWrapExpr(p)
          yield Proof.DisjunctIntroLeft(p)

        case Proof.DisjunctCommute(p) =>
          for
            p <- proofAtomicAsWrapExpr(p)
          yield Proof.DisjunctCommute(p)

        case Proof.ConjunctCommute(p) =>
          for
            p <- proofAtomicAsWrapExpr(p)
          yield Proof.ConjunctCommute(p)

        case Proof.DeMorganAndPullNotOut(p) =>
          for
            p <- proofAtomicAsWrapExpr(p)
          yield Proof.DeMorganAndPullNotOut(p)

        case Proof.DeMorganOrPullNotOut(p) =>
          for
            p <- proofAtomicAsWrapExpr(p)
          yield Proof.DeMorganOrPullNotOut(p)

        case Proof.DeMorganAndPushNotIn(p) =>
          for
            p <- proofAtomicAsWrapExpr(p)
          yield Proof.DeMorganAndPushNotIn(p)

        case Proof.DeMorganOrPushNotIn(p) =>
          for
            p <- proofAtomicAsWrapExpr(p)
          yield Proof.DeMorganOrPushNotIn(p)

        case Proof.DoubleNegIntro(p) =>
          for
            p <- proofAtomicAsWrapExpr(p)
          yield Proof.DoubleNegIntro(p)

        case Proof.Contradiction(p, notP) =>
          for
            p <- proofAtomicAsWrapExpr(p)
            notP <- proofAtomicAsWrapExpr(notP)
          yield Proof.Contradiction(p, notP)

        case Proof.HypotheticalSyllogism(pImpliesQ, qImpliesR) =>
          for
            pImpliesQ <- proofAtomicAsWrapExpr(pImpliesQ)
            qImpliesR <- proofAtomicAsWrapExpr(qImpliesR)
          yield Proof.HypotheticalSyllogism(pImpliesQ, qImpliesR)

      end match


    def resolveModel(model: Model): ZIO[R, E, Map[THole, ExprConstraints[WrapExpr]]] =
      ZIO.foreach(model) { case (hole, expr) =>
        expr.traverse(exprToWrapExpr).map { expr => (hole, expr) }
      }
  }


  protected sealed class IRPrologContext
  (
    createdHoles: Ref[Set[THole]],
    givenAssertions: Seq[ZIO[R, E, THole] => ZIO[R, E, Assertion]],
    knownVarValues: Map[TVariable, WrapExpr],
    initialFuel: Int,
  ) extends PrologContext[R, E] with IRProverContext(givenAssertions, knownVarValues, initialFuel) {
    import syntax.*


    protected override def newVariable: ZIO[R, E, ExprProverSyntax.TVariable] =
      createHole.tap { hole =>
        createdHoles.update(_ + hole)
      }

    override def compareValues[R2 <: R, E2 >: Error, T](a: Value, b: Value)(f: (ExprConstructor, Seq[Expr], Seq[Expr]) => ZStream[R2, E2, ProofResult.Yes]): ZStream[R2, E2, ProofResult.Yes] =
      (a.constructor, b.constructor) match {
        case (ExprConstructor.LoadLambda(argVariableA), ExprConstructor.LoadLambda(argVariableB)) =>
          val loadArgA = WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadVariable(argVariableA), EmptyTuple))

          ZStream.unwrap(
            ZIO.foreach(b.args)(exprToWrapExprError)
              .map { bArgs2 =>
                val bArgs3 = bArgs2.map(substituteVariables(Map(argVariableB -> loadArgA)) andThen wrapExprToExpr)

                f(a.constructor, a.args, bArgs3)
              }
          )

        case _ => super.compareValues(a, b)(f)
      }

    protected override def variableIsFromRules(variable: exprContext.THole): UIO[Boolean] =
      createdHoles.get.map(_.contains(variable))

    protected override def intrinsicPredicate
      (predicate: ExprConstructor, args: Seq[Expr], substitutions: Model, solveState: SolveState)
      : ZStream[R, Error, ProofResult.Yes] =
      (predicate, args) match {
        case (
              ExprConstructor.SubtypeWitnessType,
              Seq(Value(ExprConstructor.ClassType(classA), aArgs), Value(ExprConstructor.ClassType(classB), bArgs)),
            ) =>
          isSubClass(this, classA, aArgs, classB, bArgs, substitutions, solveState.consumeFuel)

        case (
              ExprConstructor.SubtypeWitnessType,
              Seq(Value(ExprConstructor.TraitType(traitA), aArgs), Value(ExprConstructor.TraitType(traitB), bArgs)),
            ) =>
          isSubTrait(this, traitA, aArgs, traitB, bArgs, substitutions, solveState.consumeFuel)

        case (
              ExprConstructor.SubtypeWitnessType,
              Seq(Value(ExprConstructor.ClassType(classA), aArgs), Value(ExprConstructor.TraitType(traitB), bArgs)),
            ) =>
          classImplementsTrait(this, classA, aArgs, traitB, bArgs, substitutions, solveState.consumeFuel)

        case (
              ExprConstructor.SubtypeWitnessType,
              Seq(Value(ExprConstructor.ClassType(classA), aArgs), t @ Value(ExprConstructor.TypeN | ExprConstructor.OmegaTypeN(_) | ExprConstructor.AnyType, _)),
            ) =>
          ZStream.unwrap(
            for {
              convAArgs <- ZIO.foreach(aArgs)(exprToWrapExprError)
              classT <- typeOfClass(classA, convAArgs).mapError(Left.apply)
            } yield solve(
              PredicateFunction(ExprConstructor.SubtypeWitnessType, Seq(wrapExprToExpr(classT), t)),
              substitutions,
              solveState.consumeFuel
            )
          )

        case (
              ExprConstructor.SubtypeWitnessType,
              Seq(a @ Value(ExprConstructor.LoadTuple, argsA), b @ Value(ExprConstructor.LoadTuple, argsB)),
            ) =>
          if argsA.size != argsB.size then {
            ZStream.unwrap(
              for {
                a2 <- exprToWrapExprError(a)
                b2 <- exprToWrapExprError(b)
                proof =
                  Proof.Atomic(TCAtomicProof.ExprProof(wrapExpr(
                    ExprConstructor.AssumeErasedValue,
                    EmptyTuple,
                  )))

              } yield ZStream.fail(Right(ProofResult.No(proof, substitutions)))
            )
          }
          else {
            argsA.zip(argsB)
              .map { case (argA, argB) => PredicateFunction(ExprConstructor.SubtypeWitnessType, Seq(argA, argB)) }
              .reduceOption(And.apply)
              .fold(ZStream.empty) { solve(_, substitutions, solveState) }
          }

        case (
              ExprConstructor.SubtypeWitnessType,
              Seq(
                a @ Value(ExprConstructor.TypeN, Seq(_)),
                b @ Value(ExprConstructor.OmegaTypeN(_) | ExprConstructor.AnyType, Seq()),
              ),
            ) =>
          ZStream.unwrap(
            for {
              a2 <- exprToWrapExprError(a)
              b2 <- exprToWrapExprError(b)
              proof =
                Proof.Atomic(TCAtomicProof.ExprProof(wrapExpr(
                  ExprConstructor.AssumeErasedValue,
                  EmptyTuple,
                )))

            } yield ZStream(ProofResult.Yes(proof, substitutions))
          )

        case (
              ExprConstructor.SubtypeWitnessType,
              Seq(a @ Value(ExprConstructor.OmegaTypeN(n1), Seq()), b @ Value(ExprConstructor.OmegaTypeN(n2), Seq())),
            ) =>
          if n1 <= n2 then {
            ZStream.unwrap(
              for {
                a2 <- exprToWrapExprError(a)
                b2 <- exprToWrapExprError(b)
                proof =
                  Proof.Atomic(TCAtomicProof.ExprProof(wrapExpr(
                    ExprConstructor.AssumeErasedValue,
                    EmptyTuple,
                  )))

              } yield ZStream(ProofResult.Yes(proof, substitutions))
            )
          }
          else {
            ZStream.unwrap(
              for {
                a2 <- exprToWrapExprError(a)
                b2 <- exprToWrapExprError(b)
                proof =
                  Proof.Atomic(TCAtomicProof.ExprProof(wrapExpr(
                    ExprConstructor.AssumeErasedValue,
                    EmptyTuple,
                  )))

              } yield ZStream.fail(Right(ProofResult.No(proof, substitutions)))
            )
          }

        case (
              ExprConstructor.SubtypeWitnessType,
              Seq(a @ Value(ExprConstructor.OmegaTypeN(_), Seq()), b @ Value(ExprConstructor.AnyType, Seq())),
            ) =>
          ZStream.unwrap(
            for {
              a2 <- exprToWrapExprError(a)
              b2 <- exprToWrapExprError(b)
              proof =
                Proof.Atomic(TCAtomicProof.ExprProof(wrapExpr(
                  ExprConstructor.AssumeErasedValue,
                  EmptyTuple,
                )))

            } yield ZStream(ProofResult.Yes(proof, substitutions))
          )

        case _ => ZStream.empty
      }

    protected override def mergeRelations(parentExprRelation: ExprRelation, subExprRelation: ExprRelation)
      : ExprRelation =
      (parentExprRelation, subExprRelation) match {
        case (ExprRelation.SyntacticEquality, _) | (_, ExprRelation.SyntacticEquality) => ExprRelation.SyntacticEquality
        case (ExprRelation.TypeEquality, _) | (_, ExprRelation.TypeEquality) => ExprRelation.TypeEquality
        // When checking for subtypes, use the expected variance
        case (ExprRelation.SubType, ExprRelation.SubType | ExprRelation.SuperType) => subExprRelation
        // When checking for supertypes, invert the variance
        case (ExprRelation.SuperType, ExprRelation.SuperType) => ExprRelation.SubType
        case (ExprRelation.SuperType, ExprRelation.SubType) => ExprRelation.SuperType
      }

    protected override def createConstraints(relation: ExprRelation, other: syntax.Expr): ExprConstraints[syntax.Expr] =
      relation match {
        case ExprRelation.SubType => ExprTypeBounds(subTypeBounds = Seq(other), superTypeBounds = Seq.empty)
        case ExprRelation.SuperType => ExprTypeBounds(superTypeBounds = Seq(other), subTypeBounds = Seq.empty)
        case ExprRelation.TypeEquality | ExprRelation.SyntacticEquality => ExprEqualConstraint(other)
      }

    protected override def createEqualityConstraint(other: syntax.Expr): ExprConstraints[syntax.Expr] =
      ExprEqualConstraint(other)

    protected override def swapRelation(relation: ExprRelation): ExprRelation =
      relation match {
        case ExprRelation.SubType => ExprRelation.SuperType
        case ExprRelation.SuperType => ExprRelation.SubType
        case ExprRelation.TypeEquality | ExprRelation.SyntacticEquality => relation
      }

    protected override def mergeConstraints(a: ExprConstraints[syntax.Expr], b: ExprConstraints[syntax.Expr])
      : Option[ExprConstraints[syntax.Expr]] =
      (a, b) match {
        case (ExprTypeBounds(aSuper, aSub), ExprTypeBounds(bSuper, bSub)) =>
          Some(ExprTypeBounds(aSuper ++ bSuper, aSub ++ bSub))

        case (ExprTypeBounds(_, _), fixed @ ExprEqualConstraint(_)) => Some(fixed)
        case (fixed @ ExprEqualConstraint(_), ExprTypeBounds(_, _)) => Some(fixed)

        case (ExprEqualConstraint(_), ExprEqualConstraint(_)) => None
      }

    protected override def predicateArgRelations(predicate: exprContext.ExprConstructor, arity: Int)
      : ZIO[R, E, Seq[ExprRelation]] = constructorArgRelations(predicate, arity)

    protected override def constructorArgRelations(constructor: exprContext.ExprConstructor, arity: Int)
      : ZIO[R, E, Seq[ExprRelation]] =
      constructor match {
        case ExprConstructor.ClassConstructorCall(ctor) => classConstructorRelations(ctor)
        case ExprConstructor.EnsureExecuted =>
          ZIO.succeed(Seq(ExprRelation.SyntacticEquality, ExprRelation.SyntacticEquality))
        case ExprConstructor.FunctionCall(func) => functionRelations(func)
        case ExprConstructor.FunctionObjectCall =>
          ZIO.succeed(Seq(ExprRelation.SyntacticEquality, ExprRelation.SyntacticEquality))
        case ExprConstructor.IfElse(_, _) =>
          ZIO.succeed(Seq(ExprRelation.SyntacticEquality, ExprRelation.SyntacticEquality, ExprRelation.SyntacticEquality))
        case ExprConstructor.BindVariable(_) =>
          ZIO.succeed(Seq(ExprRelation.SyntacticEquality, ExprRelation.SyntacticEquality))
        case ExprConstructor.LoadConstantBool(_) => ZIO.succeed(Seq(ExprRelation.TypeEquality))
        case ExprConstructor.LoadConstantInt(_) => ZIO.succeed(Seq(ExprRelation.TypeEquality))
        case ExprConstructor.LoadConstantString(_) => ZIO.succeed(Seq(ExprRelation.TypeEquality))
        case ExprConstructor.LoadLambda(_) => ZIO.succeed(Seq(ExprRelation.SyntacticEquality))
        case ExprConstructor.LoadTuple => ZIO.succeed(Seq.fill(arity)(ExprRelation.SubType))
        case ExprConstructor.LoadTupleElement(_) => ZIO.succeed(Seq(ExprRelation.SubType))
        case ExprConstructor.LoadVariable(_) => ZIO.succeed(Seq.empty)
        case ExprConstructor.MethodCall(method) => methodRelations(method)
        case ExprConstructor.PatternMatch(_) => ZIO.succeed(Seq.fill(arity)(ExprRelation.SyntacticEquality))
        case ExprConstructor.Proving(_) => ZIO.succeed(Seq(ExprRelation.SyntacticEquality))
        case ExprConstructor.RaiseException => ZIO.succeed(Seq(ExprRelation.SyntacticEquality))
        case ExprConstructor.Sequence => ZIO.succeed(Seq.fill(arity)(ExprRelation.SyntacticEquality))
        case ExprConstructor.StoreVariable(_) => ZIO.succeed(Seq(ExprRelation.SyntacticEquality))
        case ExprConstructor.TypeN => ZIO.succeed(Seq(ExprRelation.SyntacticEquality))
        case ExprConstructor.OmegaTypeN(_) | ExprConstructor.AnyType => ZIO.succeed(Seq.empty)
        case ExprConstructor.TraitType(arTrait) => traitRelations(arTrait)
        case ExprConstructor.ClassType(arClass) => classRelations(arClass)
        case ExprConstructor.FunctionType => ZIO.succeed(Seq(ExprRelation.SuperType, ExprRelation.SubType))
        case ExprConstructor.UnionType => ZIO.succeed(Seq(ExprRelation.SubType, ExprRelation.SubType))
        case ExprConstructor.IntersectionType => ZIO.succeed(Seq(ExprRelation.SubType, ExprRelation.SubType))
        case ExprConstructor.ExistentialType(_) => ZIO.succeed(Seq(ExprRelation.SubType))
        case ExprConstructor.ConjunctionType => ZIO.succeed(Seq(ExprRelation.SubType, ExprRelation.SubType))
        case ExprConstructor.DisjunctionType => ZIO.succeed(Seq(ExprRelation.SubType, ExprRelation.SubType))
        case ExprConstructor.NeverType => ZIO.succeed(Seq.empty)
        case ExprConstructor.SubtypeWitnessType => ZIO.succeed(Seq(ExprRelation.SuperType, ExprRelation.SubType))
        case ExprConstructor.EqualTo => ZIO.succeed(Seq(ExprRelation.SubType, ExprRelation.SyntacticEquality, ExprRelation.SyntacticEquality))
        case ExprConstructor.AssumeErasedValue => ZIO.succeed(Seq(ExprRelation.TypeEquality))
      }

    protected override def checkRelation
      (a: syntax.Expr, b: syntax.Expr, relation: ExprRelation, substitutions: Model, solveState: SolveState)
      : ZStream[R, Error, ProofResult.Yes] =
      relation match {
        case ExprRelation.SyntacticEquality => ZStream.empty
        case ExprRelation.SubType =>
          solve(PredicateFunction(ExprConstructor.SubtypeWitnessType, Seq(a, b)), substitutions, solveState)
        case ExprRelation.SuperType =>
          solve(PredicateFunction(ExprConstructor.SubtypeWitnessType, Seq(b, a)), substitutions, solveState)
        case ExprRelation.TypeEquality =>
          solve(
            And(
              PredicateFunction(ExprConstructor.SubtypeWitnessType, Seq(a, b)),
              PredicateFunction(ExprConstructor.SubtypeWitnessType, Seq(b, a)),
            ),
            substitutions,
            solveState,
          )
      }

    private def buildRelationProof(relation: ExprRelation, a: WrapExpr, b: WrapExpr): Proof[TCAtomicProof] =
      Proof.Atomic(TCAtomicProof.ExprProof(
        wrapExpr(ExprConstructor.AssumeErasedValue, EmptyTuple)
      ))

    protected override def variableRelationProof(relation: ExprRelation, a: exprContext.THole, b: exprContext.THole)
      : ZIO[R, E, Proof[TCAtomicProof]] =
      ZIO.succeed(buildRelationProof(relation, WrapExpr.OfHole(a), WrapExpr.OfHole(b)))

    protected override def variableExprRelationProof(relation: ExprRelation, a: exprContext.THole, b: syntax.Expr)
      : ZIO[R, E, Proof[TCAtomicProof]] =
      exprToWrapExpr(b).map { b2 =>
        buildRelationProof(relation, WrapExpr.OfHole(a), b2)
      }

    protected override def valueRelationProof
      (relation: ExprRelation, a: syntax.Value, b: syntax.Value, argProofs: Seq[Proof[TCAtomicProof]])
      : ZIO[R, E, Proof[TCAtomicProof]] =
      for {
        a2 <- exprToWrapExpr(a)
        b2 <- exprToWrapExpr(b)
      } yield buildRelationProof(relation, a2, b2)


    override protected def unifyCustom(goal: syntax.Predicate, rule: syntax.Predicate, model: Model, solveState: SolveState): ZStream[R, Error, Model] =
      (goal, rule) match {
        case (syntax.PredicateFunction(ExprConstructor.EqualTo, Seq(t, a1, a2)), syntax.PredicateFunction(ExprConstructor.EqualTo, Seq(u, b1, b2))) =>
          for
            ProofResult.Yes(_, model) <- unifyExpr(a1, b1, ExprRelation.SyntacticEquality, model, solveState.consumeFuel, useCustomRelations = false)
            ProofResult.Yes(_, model) <- unifyExpr(a2, b2, ExprRelation.SyntacticEquality, model, solveState.consumeFuel, useCustomRelations = false)
            ProofResult.Yes(_, model) <- solve(syntax.PredicateFunction(ExprConstructor.SubtypeWitnessType, Seq(u, t)), model, solveState.consumeFuel).take(1)
          yield model

        case _ =>
          super.unifyCustom(goal, rule, model, solveState)
      }

  }

  protected sealed class IRSmtContext
  (
    givenAssertions: Seq[ZIO[R, E, THole] => ZIO[R, E, Assertion]],
    knownVarValues: Map[TVariable, WrapExpr],
    initialFuel: Int,
  ) extends SmtContext[R, E] with IRProverContext(givenAssertions, knownVarValues, initialFuel) {
    import syntax.*

    override protected def newVariable: ZIO[R, E, exprContext.THole] = createHole

    override protected def assumeResultProof: Proof[TCAtomicProof] =
      Proof.Atomic(TCAtomicProof.ExprProof(WrapExpr.OfExpr(ArExpr(
        ExprConstructor.AssumeErasedValue,
        EmptyTuple,
      ))))

    protected override def assertions: Seq[ZIO[R, E, exprContext.THole] => ZIO[R, E, (Proof[TCAtomicProof], Predicate)]] =
      super.assertions ++ Seq(
        // A == B
        // ------
        // B == A
        newVariable => (for {
          t <- newVariable
          a <- newVariable
          b <- newVariable
        } yield (
          Proof.Atomic(TCAtomicProof.ExprProof(WrapExpr.OfExpr(ArExpr(
            ExprConstructor.AssumeErasedValue,
            EmptyTuple,
          )))) -> Implies(
            PredicateFunction(ExprConstructor.EqualTo, Seq(Variable(t), Variable(a), Variable(b))),
            PredicateFunction(ExprConstructor.EqualTo, Seq(Variable(t), Variable(b), Variable(a)))
          ))
        ),
      )


    override protected def predicateSatisfiableInModel(pf: PredicateFunction, state: ProverState): ZIO[R, E, Option[Boolean]] =
      pf match {
        case PredicateFunction(ExprConstructor.EqualTo, Seq(_, a, b)) =>
          for
            a <- normalizeExpr(a, state.model, state.fuel)
            b <- normalizeExpr(b, state.model, state.fuel)
          yield (if a == b then Some(true) else None)

        case _ =>
          super.predicateSatisfiableInModel(pf, state)
      }

    override protected def predicatesEquivalent(p1: PredicateFunction, p2: PredicateFunction, state: ProverState): ZIO[R, E, Boolean] =
      super.predicatesEquivalent(p1, p2, state) || ((p1, p2) match {
        case (PredicateFunction(ExprConstructor.EqualTo, Seq(ta, a1, a2)), PredicateFunction(ExprConstructor.EqualTo, Seq(tb, b1, b2))) if state.fuel > 1 && a1 == b1 && a2 == b2 =>
          for
            ta2 <- exprToWrapExpr(ta)
            tb2 <- exprToWrapExpr(tb)

            t1 = WrapExpr.OfExpr(ArExpr(ExprConstructor.SubtypeWitnessType, (tb2, ta2)))
            t2 = WrapExpr.OfExpr(ArExpr(ExprConstructor.SubtypeWitnessType, (ta2, tb2)))
            model <- resolveModel(state.model)
            res <-
              Ref.make(Set.empty[THole]).flatMap { createdHoles =>
                val proverContext = new IRPrologContext(createdHoles, givenAssertions, knownVarValues, state.fuel - 1)
                tryResolveWithProver(t1, model, proverContext, state.fuel - 1)
              }.map { _.isDefined } &&
                Ref.make(Set.empty[THole]).flatMap { createdHoles =>
                  val proverContext = new IRPrologContext(createdHoles, givenAssertions, knownVarValues, state.fuel - 1)
                  tryResolveWithProver(t2, model, proverContext, state.fuel - 1)
                }.map { _.isDefined }
          yield res

        case (_, _) => ZIO.succeed(false)
      })

    override protected def matchPredicateFunction(state: ProverState, pf: PredicateFunction, quantPF: PredicateFunction, quantVars: Set[THole], acc: Map[THole, Expr]): ZIO[R, Option[E], Map[THole, Expr]] =
      (pf, quantPF) match {
        case (PredicateFunction(ExprConstructor.EqualTo, Seq(ta, a1, a2)), PredicateFunction(ExprConstructor.EqualTo, Seq(tb, b1, b2))) if state.fuel > 1 =>
          for
            acc <- matchExpr(state, a1, b1, quantVars, acc)
            acc <- matchExpr(state, a2, b2, quantVars, acc)

            ta2 <- exprToWrapExpr(ta).asSomeError
            tb2 <- exprToWrapExpr(tb).asSomeError

            t = WrapExpr.OfExpr(ArExpr(ExprConstructor.SubtypeWitnessType, (tb2, ta2)))
            model <- resolveModel(state.model).asSomeError
            _ <- Ref.make(Set.empty[THole]).flatMap { createdHoles =>
              val proverContext = new IRPrologContext(createdHoles, givenAssertions, knownVarValues, state.fuel - 1)
              tryResolveWithProver(t, model, proverContext, state.fuel - 1)
            }.some
          yield acc

        case _ =>
          super.matchPredicateFunction(state, pf, quantPF, quantVars, acc)
      }
  }

  final case class ResolvedImplicit(proof: Proof[WrapExpr], model: Map[THole, ExprConstraints[WrapExpr]])

  private def tryResolveWithProver(implicitType: WrapExpr, model: Map[THole, ExprConstraints[WrapExpr]], proverContext: IRProverContext, fuel: Int): ZIO[R, E, Option[ResolvedImplicit]] =
    for
      goal <- proverContext.arExprToGoal(implicitType, fuel)
      proverModel = model.view.mapValues {
        _.map(proverContext.wrapExprToExpr)
      }.toMap

      result <- proverContext.check(goal, proverModel, fuel).flatMap {
        case proverContext.ProofResult.Yes(proof, model) =>
          for
            model <- proverContext.resolveModel(model)
            proof <- proverContext.proofAtomicAsWrapExpr(proof)
          yield Some(ResolvedImplicit(proof, model))

        case proverContext.ProofResult.No(_, _) | _: proverContext.ProofResult.Unknown.type => ZIO.none
      }

    yield result

  final case class FuelSpecifiers
  (
    evaluatorFuel: Int,
    prologFuel: Int,
    smtFuel: Int,
  )

  final def tryResolve(implicitType: WrapExpr, model: Map[THole, ExprConstraints[WrapExpr]], givenAssertions: Seq[ZIO[R, E, THole] => ZIO[R, E, Assertion]], knownVarValues: Map[TVariable, WrapExpr], fuel: FuelSpecifiers)
  : ZIO[R, E, Option[ResolvedImplicit]] =
    def prologAttempt =
      val isEqualTo = implicitType match {
        case WrapExpr.OfExpr(expr) =>
          expr.constructor match {
            case ExprConstructor.EqualTo => true
            case _ => false
          }

        case _ => false
      }

      if isEqualTo then
        ZIO.none
      else
        Ref.make(Set.empty[THole]).flatMap { createdHoles =>
          val proverContext = new IRPrologContext(createdHoles, givenAssertions, knownVarValues, fuel.prologFuel)
          tryResolveWithProver(implicitType, model, proverContext, fuel.prologFuel)
        }
    end prologAttempt


    def smtAttempt =
      val isSubType = implicitType match {
        case WrapExpr.OfExpr(expr) =>
          expr.constructor match {
            case ExprConstructor.SubtypeWitnessType => true
            case _ => false
          }

        case _ => false
      }

      if isSubType then
        ZIO.none
      else
        tryResolveWithProver(implicitType, model, new IRSmtContext(givenAssertions, knownVarValues, fuel.smtFuel), fuel.smtFuel)
    end smtAttempt

    prologAttempt.flatMap {
      case Some(res) => ZIO.some(res)
      case None => smtAttempt
    }
  end tryResolve
}
