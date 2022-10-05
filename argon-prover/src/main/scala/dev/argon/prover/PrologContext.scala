package dev.argon.prover

import dev.argon.util.{*, given}
import zio.*
import zio.stream.{Stream, ZStream}
import dev.argon.util.UniqueIdentifier

abstract class PrologContext[-R, +E] {
  val syntax: PrologSyntax
  import syntax.*

  type ProofAtom

  // Relation is for comparing expressions as arguments to a constructor (ex: A <: B, A =:= B, A = B)
  type TRelation
  type TConstraints

  type Model = Map[syntax.TVariable, TConstraints]

  sealed trait PrologResult

  object PrologResult {
    final case class Yes(proof: Proof[ProofAtom], model: Model) extends PrologResult
    final case class No(notProof: Proof[ProofAtom], model: Model) extends PrologResult
    case object Unknown extends PrologResult

    given CanEqual[Unknown.type, PrologResult] = CanEqual.canEqualAny
    given CanEqual[PrologResult, Unknown.type] = CanEqual.canEqualAny
  }

  protected def assertions: ZIO[R, E, List[(Proof[ProofAtom], Predicate)]]

  protected def variableIsFromRules(variable: TVariable): UIO[Boolean]

  protected def intrinsicPredicate(predicate: TPredicateFunction, args: Seq[Expr], substitutions: Model, solveState: SolveState)
    : ZStream[R, Error, PrologResult.Yes]

  protected def normalize(expr: Value, substitutions: Model, solveState: SolveState): ZIO[R, E, Expr]

  // Combine relations when checking a sub expression
  protected def mergeRelations(parentExprRelation: TRelation, subExprRelation: TRelation): TRelation
  // Swaps the relation to handle arguments in the opposite order (eg. (<=) => (>=), (=) => (=))
  protected def swapRelation(relation: TRelation): TRelation
  // Create a constraint (eg. (= 5), (< 7), etc.)
  protected def createConstraints(relation: TRelation, other: syntax.Expr): TConstraints
  protected def createEqualityConstraint(other: syntax.Expr): TConstraints
  protected def mergeConstraints(a: TConstraints, b: TConstraints): Option[TConstraints]
  protected def predicateArgRelations(predicate: syntax.TPredicateFunction, arity: Int): ZIO[R, E, Seq[TRelation]]
  protected def constructorArgRelations(constructor: syntax.TConstructor, arity: Int): ZIO[R, E, Seq[TRelation]]

  // Checks whether the relation holds between two expressions.
  protected def checkRelation(a: Expr, b: Expr, relation: TRelation, substitutions: Model, solveState: SolveState)
    : ZStream[R, Error, PrologResult.Yes]

  protected def otherForEquivalenceRelation(constraints: TConstraints): Option[syntax.Expr]
  protected def variableRelationProof(relation: TRelation, a: TVariable, b: TVariable): ZIO[R, E, Proof[ProofAtom]]
  protected def variableExprRelationProof(relation: TRelation, a: TVariable, b: syntax.Expr): ZIO[R, E, Proof[ProofAtom]]

  protected def valueRelationProof
    (relation: TRelation, a: syntax.Value, b: syntax.Value, argProofs: Seq[Proof[ProofAtom]])
    : ZIO[R, E, Proof[ProofAtom]]

  private type Error = Either[E, PrologResult.No]

  final def check(goal: Predicate, fuel: Int): ZIO[R, E, PrologResult] = check(goal, Map.empty, fuel)

  final def check(goal: Predicate, model: Model, fuel: Int): ZIO[R, E, PrologResult] =
    solve(goal, model, SolveState(seenPredicates = Set.empty, fuel = fuel, additionalGivens = Seq.empty))
      .runHead
      .foldZIO(
        failure = {
          case Left(e) => ZIO.fail(e)
          case Right(no) => ZIO.succeed(no)
        },
        success = {
          case Some(result) => ZIO.succeed(result)
          case None => ZIO.succeed(PrologResult.Unknown)
        },
      )

  private def emptyIfNo[A](stream: ZStream[R, Error, A]): ZStream[R, Either[E, Nothing], A] =
    stream.catchAll {
      case Left(e) => ZStream.fail(Left(e))
      case Right(PrologResult.No(_, _)) => ZStream.empty
    }

  // Converts:
  //   A disproof of P into a proof of not P
  //   A proof of P into a disproof of not P
  private def invertProof(stream: ZStream[R, Error, PrologResult.Yes]): ZStream[R, Error, PrologResult.Yes] =
    ZStream.unwrap(
      stream
        .runHead
        .fold(
          failure = {
            case Left(e) => ZStream.fail(Left(e))
            case Right(PrologResult.No(notProof, model)) => ZStream.succeed(PrologResult.Yes(notProof, model))
          },
          success = {
            case Some(PrologResult.Yes(proof, model)) => ZStream.fail(Right(PrologResult.No(Proof.DoubleNegIntro(proof), model)))
            case None => ZStream.empty
          }
        )
    )


  final case class SolveState
  (
    seenPredicates: Set[Predicate],
    fuel: Int,
    additionalGivens: Seq[(Proof[ProofAtom], Predicate)],
  ) {
    def consumeFuel: SolveState =
      copy(fuel = fuel - 1)

    def fuelEmpty: Boolean =
      fuel <= 0

    def addPredicate(pred: Predicate): SolveState =
      copy(seenPredicates = seenPredicates + pred)

    def hasSeenPredicate(pred: Predicate, model: Model): Boolean =
      seenPredicates.exists(seenPred => quickEqualPredicate(model)(seenPred, pred))

    def addGiven(proof: Proof[ProofAtom], pred: Predicate): SolveState =
      copy(additionalGivens = additionalGivens :+ (proof, pred))
  }


  def solve(goal: Predicate, substitutions: Model, solveState: SolveState): ZStream[R, Error, PrologResult.Yes] =
    if solveState.fuelEmpty || solveState.hasSeenPredicate(goal, substitutions) then
      ZStream.empty
    else
      val solveState2 = solveState.addPredicate(goal)

      inferAll(goal, substitutions, solveState2) ++ (goal match {
        case And(a, b) =>
          solve(a, substitutions, solveState2.consumeFuel)
            .mapError {
              _.map { case PrologResult.No(proof, model) =>
                PrologResult.No(Proof.DeMorganOrPullNotOut(Proof.DisjunctIntroLeft(proof)), model)
              }
            }
            .flatMap { case PrologResult.Yes(proofA, substitutions) =>
              solve(b, substitutions, solveState2.consumeFuel)
                .mapError {
                  _.map { case PrologResult.No(proof, model) =>
                    PrologResult.No(Proof.DeMorganOrPullNotOut(Proof.DisjunctIntroRight(proof)), model)
                  }
                }
                .map { case PrologResult.Yes(proofB, substitutions) =>
                  PrologResult.Yes(Proof.ConjunctIntro(proofA, proofB), substitutions)
                }
            }

        case Or(a, b) =>
          val commuteOr =
            inferAll(Or(b, a), substitutions, solveState2.consumeFuel)
              .mapError {
                _.map {
                  case PrologResult.No(proof, model) =>
                    PrologResult.No(
                      Proof.DeMorganAndPullNotOut(Proof.ConjunctCommute(Proof.DeMorganOrPushNotIn(proof))),
                      model,
                    )
                }
              }
              .map { case PrologResult.Yes(proof, substitutions) =>
                PrologResult.Yes(Proof.DisjunctCommute(proof), substitutions)
              }

          commuteOr
            ++ emptyIfNo(solve(a, substitutions, solveState2.consumeFuel)).map { case PrologResult.Yes(proof, substitutions) =>
              PrologResult.Yes(Proof.DisjunctIntroLeft(proof), substitutions)
            }
            ++ emptyIfNo(solve(b, substitutions, solveState2.consumeFuel)).map { case PrologResult.Yes(proof, substitutions) =>
              PrologResult.Yes(Proof.DisjunctIntroRight(proof), substitutions)
            }

        case Implies(And(a, b), PropFalse) =>
          solve(Or(Implies(a, PropFalse), Implies(b, PropFalse)), substitutions, solveState2.consumeFuel)
            .catchSome {
              case Right(PrologResult.No(proof, model)) =>
                ZStream.fromZIO(
                  UniqueIdentifier.make.flatMap { id =>
                    ZIO.fail(Right(PrologResult.No(
                      Proof.HypotheticalSyllogism(
                        Proof.ImplicaitonAbstraction(id, Proof.DeMorganAndPushNotIn(Proof.Identifier(id))),
                        proof,
                      ),
                      model,
                    )))
                  }
                )
            }
            .map { case PrologResult.Yes(proof, substitutions) =>
              PrologResult.Yes(Proof.DeMorganOrPullNotOut(proof), substitutions)
            }

        case Implies(Or(a, b), PropFalse) =>
          solve(And(Implies(a, PropFalse), Implies(b, PropFalse)), substitutions, solveState2.consumeFuel).map {
            case PrologResult.Yes(proof, substitutions) =>
              PrologResult.Yes(Proof.DeMorganAndPullNotOut(proof), substitutions)
          }

        case Implies(Implies(a, PropFalse), PropFalse) => solve(a, substitutions, solveState2.consumeFuel).map {
            case PrologResult.Yes(proof, substitutions) => PrologResult.Yes(Proof.DoubleNegIntro(proof), substitutions)
          }

        case Implies(a, PropFalse) =>
          invertProof(solve(a, substitutions, solveState2.consumeFuel))

        case Implies(a, b) =>
          ZStream.unwrap(
            for
              aId <- UniqueIdentifier.make
            yield solve(b, substitutions, solveState.addGiven(Proof.Identifier(aId), a))
              .map { case PrologResult.Yes(proof, substitutions) =>
                PrologResult.Yes(Proof.ImplicaitonAbstraction(aId, proof), substitutions)
              }
          )

        case PropFalse => ZStream.empty

        case PredicateFunction(function, args) =>
          intrinsicPredicate(function, args, substitutions, solveState2.consumeFuel)
      })
    end if



  private def inferAll(goal: Predicate, substitutions: Model, solveState: SolveState): ZStream[R, Error, PrologResult.Yes] =
    if solveState.fuelEmpty then
      ZStream.empty
    else
      ZStream.unwrap(
        for {
          kbUnsorted <- assertions.mapError(Left.apply)
        } yield infer(goal, substitutions, solveState, kbUnsorted)
      )

  private def infer(goal: Predicate, substitutions: Model, solveState: SolveState, kb: Seq[(Proof[ProofAtom], Predicate)])
    : ZStream[R, Error, PrologResult.Yes] =
    ZStream.fromIterable(kb ++ solveState.additionalGivens).flatMap { (proof, assertion) =>
      inferOne(goal, assertion, substitutions, solveState, proof)
    }

  private def inferOne(goal: Predicate, assertion: Predicate, substitutions: Model, solveState: SolveState, proof: Proof[ProofAtom])
    : ZStream[R, Error, PrologResult.Yes] = {
    type ErrorF = Either[E, (Proof[ProofAtom] => Proof[ProofAtom], Model)]

    def impl(assertion: Predicate): ZStream[R, ErrorF, (Proof[ProofAtom] => Proof[ProofAtom], Model)] =
      val unifyAssertion: ZStream[R, ErrorF, (Proof[ProofAtom] => Proof[ProofAtom], Model)] =
        emptyIfNo(unify(goal, assertion, substitutions, solveState))
          .map { model =>
            (
              (proof: Proof[ProofAtom]) => proof,
              model
            )
          }

      val assertionSpecific: ZStream[R, ErrorF, (Proof[ProofAtom] => Proof[ProofAtom], Model)] =
        assertion match {
          case Implies(PropFalse, _) => ZStream.empty

          case Implies(premise, PropFalse) =>
            emptyIfNo(unify(goal, premise, substitutions, solveState.consumeFuel))
              .flatMap { substitutions =>
                ZStream.fail(Right((
                  (proof: Proof[ProofAtom]) => proof,
                  substitutions
                )))
              }

          case Implies(premise, conclusion) =>
            impl(conclusion)
              .catchSome({
                case Right((buildNotResult, substitutions)) =>
                  emptyIfNo(solve(premise, substitutions, solveState.consumeFuel))
                    .flatMap { case PrologResult.Yes(premiseProof, substitutions) =>
                      ZStream.fail(Right((
                        (proof: Proof[ProofAtom]) => Proof.ModusPonens(buildNotResult(proof), premiseProof),
                        substitutions
                      )))
                    }
              })
              .flatMap { (buildResult, substitutions) =>
                emptyIfNo(solve(premise, substitutions, solveState.consumeFuel))
                .map { case PrologResult.Yes(premiseProof, substitutions) =>
                  (
                    (proof: Proof[ProofAtom]) => Proof.ModusPonens(buildResult(proof), premiseProof),
                    substitutions
                  )
                }
              }

          case _ => ZStream.empty
        }

      unifyAssertion ++ assertionSpecific
    end impl


    goal match {
      case Implies(_, _) =>
        ZStream.empty

      case _ =>
        impl(assertion)
          .mapBoth(
            {
              case Left(e) => Left(e)
              case Right((buildResult, substitutions)) =>
                Right(PrologResult.No(buildResult(proof), substitutions))
            },
            { (buildResult, substitutions) => PrologResult.Yes(buildResult(proof), substitutions) }
          )
    }
  }

  private def quickEqualExpr(model: Model)(a: Expr, b: Expr): Boolean =
    (a, b) match {
      case (Value(ca, argsA), Value(cb, argsB)) if ca == cb && argsA == argsB =>
        argsA.zip(argsB).forall(quickEqualExpr(model))

      case (Value(_, _), _) | (_, Value(_, _)) => false

      case (Variable(va), Variable(vb)) =>
        (model.get(va), model.get(vb)) match {
          case (None, None) => true
          case (Some(consA), None) =>
            otherForEquivalenceRelation(consA) match {
              case Some(otherA) => quickEqualExpr(model)(otherA, b)
              case None => a == b
            }

          case (None, Some(consB)) =>
            otherForEquivalenceRelation(consB) match {
              case Some(otherB) => quickEqualExpr(model)(a, otherB)
              case None => a == b
            }

          case (Some(consA), Some(consB)) =>
            (otherForEquivalenceRelation(consA), otherForEquivalenceRelation(consB)) match {
              case (Some(otherA), Some(otherB)) => quickEqualExpr(model)(otherA, otherB)
              case (Some(otherA), None) => quickEqualExpr(model)(otherA, b)
              case (None, Some(otherB)) => quickEqualExpr(model)(a, otherB)
              case (None, None) => false
            }
        }
    }

  private def quickEqualPredicate(model: Model)(a: Predicate, b: Predicate): Boolean =
    (a, b) match {
      case (PredicateFunction(fa, argsA), PredicateFunction(fb, argsB)) if fa == fb && argsA.size == argsB.size =>
        argsA.zip(argsB).forall(quickEqualExpr(model))

      case (PredicateFunction(_, _), _) | (_, PredicateFunction(_, _)) => false

      case (And(a1, a2), And(b1, b2)) =>
        quickEqualPredicate(model)(a1, b1) && quickEqualPredicate(model)(a2, b2)

      case (And(_, _), _) | (_, And(_, _)) => false

      case (Or(a1, a2), Or(b1, b2)) =>
        quickEqualPredicate(model)(a1, b1) && quickEqualPredicate(model)(a2, b2)

      case (Or(_, _), _) | (_, Or(_, _)) => false

      case (Implies(a1, a2), Implies(b1, b2)) =>
        quickEqualPredicate(model)(a1, b1) && quickEqualPredicate(model)(a2, b2)

      case (Implies(_, _), _) | (_, Implies(_, _)) => false

      case (PropFalse, PropFalse) =>
        true
    }

  protected final def unify(goal: Predicate, rule: Predicate, model: Model, solveState: SolveState): ZStream[R, Error, Model] =
    if solveState.fuelEmpty then
      ZStream.empty
    else
      unifyCustom(goal, rule, model, solveState)

  protected def unifyCustom(goal: Predicate, rule: Predicate, model: Model, solveState: SolveState): ZStream[R, Error, Model] =
    (goal, rule) match {
      case (PredicateFunction(f1, args1), PredicateFunction(f2, args2))
        if f1 == f2 && args1.size == args2.size =>
        ZStream.unwrap(
          predicateArgRelations(f1, args1.size)
            .mapError(Left.apply)
            .map { relations =>
              if relations.size != args1.size then
                ZStream.empty
              else
                args1.zip(args2).zip(relations).toList.foldLeftM(model) {
                  case (model, ((arg1, arg2), argRelation)) =>
                    unifyExpr(arg1, arg2, argRelation, model, solveState, useCustomRelations = false)
                      .map { case PrologResult.Yes(_, model) => model }
                }
            }
        )

      case (And(left1, right1), And(left2, right2)) =>
        unify(left1, left2, model, solveState).flatMap { model =>
          unify(right1, right2, model, solveState)
        }

      case (Or(left1, right1), Or(left2, right2)) =>
        unify(left1, left2, model, solveState).flatMap { model =>
          unify(right1, right2, model, solveState)
        }

      case (Implies(left1, right1), Implies(left2, right2)) =>
        unify(left1, left2, model, solveState).flatMap { model =>
          unify(right1, right2, model, solveState)
        }

      case (PropFalse, PropFalse) => ZStream.succeed(model)

      case _ => ZStream.empty
    }

  protected def normalizeExpr(e: Expr, substitutions: Model, solveState: SolveState): ZIO[R, E, Expr] =
    def impl(e: Expr): ZIO[R, E, Expr] =
      e match {
        case value: Value => normalize(value, substitutions, solveState)
        case Variable(variable) =>
          substitutions.get(variable).flatMap(otherForEquivalenceRelation) match {
            case Some(value) => normalizeExpr(value, substitutions, solveState.consumeFuel)
            case None => ZIO.succeed(e)
          }
      }

    impl(e)
  end normalizeExpr

  def compareValues[R2 <: R, E2 >: Error, T](a: Value, b: Value)(f: (TConstructor, Seq[Expr], Seq[Expr]) => ZStream[R2, E2, PrologResult.Yes]): ZStream[R2, E2, PrologResult.Yes] =
    if a.constructor == b.constructor then
      f(a.constructor, a.args, b.args)
    else
      ZStream.empty


  protected final def unifyExpr
    (goal: Expr, rule: Expr, relation: TRelation, substitutions: Model, solveState: SolveState, useCustomRelations: Boolean)
    : ZStream[R, Error, PrologResult.Yes] =
    if solveState.fuelEmpty then
      ZStream.empty
    else
      def customRelations = checkRelation(goal, rule, relation, substitutions, solveState.consumeFuel)

      def unifyVariable(variable: TVariable, other: syntax.Expr, relation: TRelation)
        : ZStream[R, Error, PrologResult.Yes] =
        ZStream.unwrap(
          variableIsFromRules(variable).map { fromRules =>
            val constraints =
              if fromRules then createEqualityConstraint(other)
              else createConstraints(relation, other)

            addSubstitution(substitutions, variable, constraints)
              .mapZIO { substitutions =>
                variableExprRelationProof(relation, variable, other)
                  .mapError(Left.apply)
                  .map { proof =>
                    PrologResult.Yes(proof, substitutions)
                  }
              }
          }
        )

      ZStream.unwrap(
        normalizeExpr(goal, substitutions, solveState).mapError(Left.apply).flatMap { goal =>
          normalizeExpr(rule, substitutions, solveState).mapError(Left.apply).map { rule =>
            (goal, rule) match {
              case (v1: Value, v2: Value) =>
                compareValues(v1, v2) { (ctor, args1, args2) =>
                  ZStream.unwrap(
                    constructorArgRelations(ctor, args1.size)
                      .mapError(Left.apply)
                      .map { relations =>
                        args1.zip(args2).zip(relations).toList.foldLeftM((substitutions, Seq.empty[Proof[ProofAtom]])) {
                          case ((substitutions, argProofs), ((arg1, arg2), argRelation)) =>
                            val mergedRel = mergeRelations(relation, argRelation)

                            unifyExpr(arg1, arg2, mergedRel, substitutions, solveState.consumeFuel, useCustomRelations = true)
                              .map { case PrologResult.Yes(argProof, substitutions) =>
                                (substitutions, argProofs :+ argProof)
                              }
                        }
                          .mapZIO { case (substitutions, argProofs) =>
                            valueRelationProof(relation, v1, v2, argProofs)
                              .mapError(Left.apply)
                              .map { proof => PrologResult.Yes(proof, substitutions) }
                          }
                      }
                  ) ++ (if useCustomRelations then customRelations else ZStream.empty)
                }.take(1)

              case (Variable(var1), Variable(var2)) if var1 == var2 =>
                ZStream.fromZIO(
                  variableRelationProof(relation, var1, var2)
                    .mapError(Left.apply)
                    .map { proof =>
                      PrologResult.Yes(proof, substitutions)
                    }
                )

              case (Variable(var1), other) =>
                unifyVariable(var1, other, relation)

              case (other, Variable(var2)) =>
                unifyVariable(var2, other, swapRelation(relation))

              case _ => ZStream.empty
            }
          }
        }
      )
    end if
  end unifyExpr

  private def addSubstitution(substitutions: Model, variable: TVariable, constraints: TConstraints)
    : ZStream[R, Error, Model] =
    ZStream.fromIterable(
      substitutions.get(variable)
        .fold(Some(constraints))(mergeConstraints(_, constraints))
        .map { constraints =>
          substitutions + (variable -> constraints)
        }
        .toList
    )

}

object PrologContext {
  type Aux[R, E, TSyntax <: PrologSyntax] = PrologContext[R, E] { val syntax: TSyntax }
}
