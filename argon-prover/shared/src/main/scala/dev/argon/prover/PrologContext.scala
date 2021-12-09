package dev.argon.prover

import dev.argon.util.{_, given}
import zio._
import zio.stream.{Stream, ZStream}
import dev.argon.util.UniqueIdentifier

abstract class PrologContext[R <: Random, E] {
  val syntax: PrologSyntax
  import syntax._

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

  protected def intrinsicPredicate(predicate: TPredicateFunction, args: Seq[Expr], substitutions: Model, fuel: Int)
    : ZStream[R, Error, PrologResult.Yes]

  protected def normalize(expr: Expr, fuel: Int): ZIO[R, E, Expr]

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
  protected def checkRelation(a: syntax.Expr, b: syntax.Expr, relation: TRelation, substitutions: Model, fuel: Int)
    : ZStream[R, Error, PrologResult.Yes]

  protected def otherForEquivalenceRelation(constraints: TConstraints): Option[syntax.Expr]
  protected def variableRelationProof(relation: TRelation, a: TVariable, b: TVariable): ZIO[R, E, Proof[ProofAtom]]
  protected def variableExprRelationProof(relation: TRelation, a: TVariable, b: syntax.Expr): ZIO[R, E, Proof[ProofAtom]]

  protected def valueRelationProof
    (relation: TRelation, a: syntax.Value, b: syntax.Value, argProofs: Seq[Proof[ProofAtom]])
    : ZIO[R, E, Proof[ProofAtom]]

  protected type Error = Either[E, PrologResult.No]

  protected def resultIOToStream(io: ZIO[R, E, PrologResult]): ZStream[R, Error, PrologResult.Yes] =
    ZStream.unwrap(
      io
        .mapError(Left.apply)
        .flatMap {
          case _: PrologResult.Unknown.type => IO.succeed(Stream.empty)
          case result: PrologResult.No => IO.fail(Right(result))
          case result: PrologResult.Yes => IO.succeed(ZStream(result))
        }
    )

  final def check(goal: Predicate, fuel: Int): ZIO[R, E, PrologResult] = check(goal, Map.empty, fuel)

  final def check(goal: Predicate, model: Model, fuel: Int): ZIO[R, E, PrologResult] =
    solve(goal, model, fuel)
      .runHead
      .foldZIO(
        failure = {
          case Left(e) => IO.fail(e)
          case Right(no) => IO.succeed(no)
        },
        success = {
          case Some(result) => IO.succeed(result)
          case None => IO.succeed(PrologResult.Unknown)
        },
      )

  private def emptyIfNo(stream: ZStream[R, Error, PrologResult.Yes]): ZStream[R, Error, PrologResult.Yes] =
    stream.catchSome {
      case Right(PrologResult.No(_, _)) => Stream.empty
    }

  protected def solve(goal: Predicate, substitutions: Model, fuel: Int): ZStream[R, Error, PrologResult.Yes] =
    inferAll(goal, substitutions, fuel) ++ (goal match {
      case And(a, b) =>
        solve(a, substitutions, fuel)
          .mapError {
            _.map { case PrologResult.No(proof, model) =>
              PrologResult.No(Proof.DeMorganOrPullNotOut(Proof.DisjunctIntroLeft(proof)), model)
            }
          }
          .flatMap { case PrologResult.Yes(proofA, substitutions) =>
            solve(b, substitutions, fuel)
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
          inferAll(Or(b, a), substitutions, fuel)
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
          ++ emptyIfNo(solve(a, substitutions, fuel)).map { case PrologResult.Yes(proof, substitutions) =>
            PrologResult.Yes(Proof.DisjunctIntroLeft(proof), substitutions)
          }
          ++ emptyIfNo(solve(b, substitutions, fuel)).map { case PrologResult.Yes(proof, substitutions) =>
            PrologResult.Yes(Proof.DisjunctIntroRight(proof), substitutions)
          }

      case Implies(And(a, b), PropFalse) =>
        solve(Or(Implies(a, PropFalse), Implies(b, PropFalse)), substitutions, fuel)
          .catchSome {
            case Right(PrologResult.No(proof, model)) =>
              ZStream.fromEffect(
                UniqueIdentifier.make.flatMap { id =>
                  IO.fail(Right(PrologResult.No(
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
        solve(And(Implies(a, PropFalse), Implies(b, PropFalse)), substitutions, fuel).map {
          case PrologResult.Yes(proof, substitutions) =>
            PrologResult.Yes(Proof.DeMorganAndPullNotOut(proof), substitutions)
        }

      case Implies(Implies(a, PropFalse), PropFalse) => solve(a, substitutions, fuel).map {
          case PrologResult.Yes(proof, substitutions) => PrologResult.Yes(Proof.DoubleNegIntro(proof), substitutions)
        }

      case PropFalse =>
        ZStream.unwrap(
          for {
            kbUnsorted <- assertions.mapError(Left.apply)
            kb <- Random.shuffle(kbUnsorted)
          } yield ZStream.fromIterable(kb)
        ).flatMap { case (proof, predicate) =>
          emptyIfNo(
            solve(Implies(predicate, PropFalse), substitutions, fuel - 1)
              .map { case PrologResult.Yes(notProof, substitutions) =>
                PrologResult.Yes(Proof.Contradiction(proof, notProof), substitutions)
              }
          )
        }

      case PredicateFunction(function, args) =>
        intrinsicPredicate(function, args, substitutions, fuel - 1)

      case _ => Stream.empty
    })

  private def inferAll(goal: Predicate, substitutions: Model, fuel: Int): ZStream[R, Error, PrologResult.Yes] =
    ZStream.unwrap(
      for {
        kbUnsorted <- assertions.mapError(Left.apply)
        kb <- Random.shuffle(kbUnsorted)
      } yield infer(goal, substitutions, kb, fuel)
    )

  private def infer(goal: Predicate, substitutions: Model, kb: List[(Proof[ProofAtom], Predicate)], fuel: Int)
    : ZStream[R, Error, PrologResult.Yes] =
    kb match {
      case _ if fuel <= 0 => Stream.empty
      case _: Nil.type => Stream.empty
      case (proof, assertion) :: t =>
        inferOne(goal, assertion, substitutions, proof, fuel) ++ infer(goal, substitutions, t, fuel)

    }

  private def inferOne(goal: Predicate, assertion: Predicate, substitutions: Model, proof: Proof[ProofAtom], fuel: Int)
    : ZStream[R, Error, PrologResult.Yes] = {
    val unifyAssertion =
      unify(goal, assertion, substitutions, fuel)
        .map { model => PrologResult.Yes(proof, model) }

    val assertionSpecific =
      assertion match {
        case Implies(premise, conclusion) =>
          val implicationCheck =
            premise match {
              case PropFalse => Stream.empty
              case _ =>
                emptyIfNo(
                  unify(goal, conclusion, substitutions, fuel)
                    .flatMap { substitutions =>
                      solve(premise, substitutions, fuel - 1)
                        .map { case PrologResult.Yes(premiseProof, substitutions) =>
                          PrologResult.Yes(Proof.ModusPonens(proof, premiseProof), substitutions)
                        }
                    }
                )
            }

          val notCheck =
            conclusion match {
              case PropFalse =>
                unify(goal, premise, substitutions, fuel)
                  .flatMap { model =>
                    Stream.fail(Right(PrologResult.No(proof, model)))
                  }

              case _ => Stream.empty
            }

          implicationCheck ++ notCheck

        case _ => Stream.empty
      }

    assertionSpecific ++ unifyAssertion
  }

  private def unify(goal: Predicate, rule: Predicate, substitutions: Model, fuel: Int): ZStream[R, Error, Model] =
    (goal, rule) match {
      case (pf1 @ PredicateFunction(f1, args1), pf2 @ PredicateFunction(f2, args2))
          if f1 == f2 && args1.size == args2.size =>
        ZStream.unwrap(
          predicateArgRelations(f1, args1.size)
            .mapError(Left.apply)
            .map { relations =>
              if relations.size != args1.size then
                Stream.empty
              else
                args1.zip(args2).zip(relations).toList.foldLeftM(substitutions) {
                  case (substitutions, ((arg1, arg2), argRelation)) =>
                    unifyExpr(arg1, arg2, argRelation, substitutions, fuel, useCustomRelations = false)
                      .map { case PrologResult.Yes(_, substitutions) => substitutions }
                }
            }
        )

      case (And(left1, right1), And(left2, right2)) =>
        unify(left1, left2, substitutions, fuel).flatMap { substitutions =>
          unify(right1, right2, substitutions, fuel)
        }

      case (Or(left1, right1), Or(left2, right2)) =>
        unify(left1, left2, substitutions, fuel).flatMap { substitutions =>
          unify(right1, right2, substitutions, fuel)
        }

      case (Implies(left1, right1), Implies(left2, right2)) =>
        unify(left1, left2, substitutions, fuel).flatMap { substitutions =>
          unify(right1, right2, substitutions, fuel)
        }

      case (PropFalse, PropFalse) => Stream.succeed(substitutions)

      case _ => Stream.empty
    }

  private def normalizeExpr(e: Expr, substitutions: Model, fuel: Int): ZIO[R, E, Expr] =
    def impl(e: Expr): ZIO[R, E, Expr] =
      e match {
        case value: Value => normalize(value, fuel)
        case Variable(variable) =>
          substitutions.get(variable).flatMap(otherForEquivalenceRelation) match {
            case Some(value) => normalizeExpr(value, substitutions, fuel)
            case None => IO.succeed(e)
          }
      }

    impl(e)
  end normalizeExpr

  private def unifyExpr
    (goal: Expr, rule: Expr, relation: TRelation, substitutions: Model, fuel: Int, useCustomRelations: Boolean)
    : ZStream[R, Error, PrologResult.Yes] =
    if fuel <= 0 then
      Stream.empty
    else
      val customRelations = checkRelation(goal, rule, relation, substitutions, fuel - 1)

      def unifyVariable(variable: TVariable, other: syntax.Expr, relation: TRelation)
        : ZStream[R, Error, PrologResult.Yes] =
        ZStream.unwrap(
          variableIsFromRules(variable).map { fromRules =>
            val constraints =
              if fromRules then createEqualityConstraint(other)
              else createConstraints(relation, other)

            val unifiedResult =
              addSubstitution(substitutions, variable, constraints)
                .mapM { substitutions =>
                  variableExprRelationProof(relation, variable, other)
                    .mapError(Left.apply)
                    .map { proof =>
                      PrologResult.Yes(proof, substitutions)
                    }
                }

            unifiedResult ++ (if fromRules then Stream.empty else customRelations)
          }
        )

      ZStream.unwrap(
        normalizeExpr(goal, substitutions, fuel).mapError(Left.apply).flatMap { goal =>
          normalizeExpr(rule, substitutions, fuel).mapError(Left.apply).map { rule =>
            (goal, rule) match {
              case (v1 @ Value(ctor1, args1), v2 @ Value(ctor2, args2)) if ctor1 == ctor2 =>
                ZStream.unwrap(
                  constructorArgRelations(ctor1, args1.size)
                    .mapError(Left.apply)
                    .map { relations =>
                      args1.zip(args2).zip(relations).toList.foldLeftM((substitutions, Seq.empty[Proof[ProofAtom]])) {
                        case ((substitutions, argProofs), ((arg1, arg2), argRelation)) =>
                          val mergedRel = mergeRelations(relation, argRelation)

                          unifyExpr(arg1, arg2, mergedRel, substitutions, fuel, useCustomRelations = true)
                            .map { case PrologResult.Yes(argProof, substitutions) =>
                              (substitutions, argProofs :+ argProof)
                            }
                      }
                        .mapM { case (substitutions, argProofs) =>
                          valueRelationProof(relation, v1, v2, argProofs)
                            .mapError(Left.apply)
                            .map { proof => PrologResult.Yes(proof, substitutions) }
                        }
                    }
                ) ++ customRelations

              case (Variable(var1), Variable(var2)) if var1 == var2 =>
                Stream.fromEffect(
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

              case _ => Stream.empty
            }
          }
        }
      )
    end if
  end unifyExpr

  private def addSubstitution(substitutions: Model, variable: TVariable, constraints: TConstraints)
    : ZStream[R, Error, Model] =
    Stream.fromIterable(
      substitutions.get(variable)
        .fold(Some(constraints))(mergeConstraints(_, constraints))
        .map { constraints =>
          substitutions + (variable -> constraints)
        }
        .toList
    )

}

object PrologContext {
  type Aux[R <: Random, E, TSyntax <: PrologSyntax] = PrologContext[R, E] { val syntax: TSyntax }
}
