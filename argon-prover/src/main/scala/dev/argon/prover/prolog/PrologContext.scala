package dev.argon.prover.prolog

import dev.argon.prover.*
import dev.argon.util.{*, given}
import zio.*
import zio.stream.*
import cats.data.OptionT
import zio.interop.catz.core.given

abstract class PrologContext[R, E] extends ProverContext[R, E] {
  import syntax.*

  protected def intrinsicPredicate(predicate: TPredicateExpr, model: Model, solveState: SolveState)
    : ZStream[R, E, ProofResult.Definitive]

  final def check(goal: Predicate, model: Model, fuel: Fuel): ZIO[R, E, ProofResult] =
    solve(goal, model, SolveState(seenPredicates = Set.empty, fuel = fuel, additionalGivens = Seq.empty))
      .runHead
      .map {
        case Some(res) => res
        case None => ProofResult.Unknown
      }

  final case class SolveState
  (
    seenPredicates: Set[Predicate],
    fuel: Fuel,
    additionalGivens: Seq[(Proof[ProofAtom], Predicate)],
  ) {
    def consumeFuel: SolveState =
      copy(fuel = fuel.consume)

    def fuelEmpty: Boolean =
      fuel.isEmpty

    def addPredicate(pred: Predicate): SolveState =
      copy(seenPredicates = seenPredicates + pred)

    def hasSeenPredicate(pred: Predicate, model: Model): Boolean =
      seenPredicates.contains(pred)

    def addGiven(proof: Proof[ProofAtom], pred: Predicate): SolveState =
      copy(additionalGivens = additionalGivens :+ (proof, pred))
  }


  def solve(goal: Predicate, model: Model, solveState: SolveState): ZStream[R, E, ProofResult.Definitive] =
    if solveState.fuelEmpty || solveState.hasSeenPredicate(goal, model) then
      ZStream.empty
    else
      val solveState2 = solveState.addPredicate(goal)

      infer(goal, model, solveState2) ++ (goal match {
        case And(a, b) =>
          solve(a, model, solveState2.consumeFuel)
            .flatMap {
              case ProofResult.No(notProof, model) =>
                ZStream.succeed(ProofResult.No(Proof.DeMorganOrPullNotOut(Proof.DisjunctIntroLeft(notProof)), model))

              case ProofResult.Yes(proofA, model) =>
                solve(b, model, solveState2.consumeFuel)
                  .map {
                    case ProofResult.No(proof, model) =>
                      ProofResult.No(Proof.DeMorganOrPullNotOut(Proof.DisjunctIntroRight(proof)), model)

                    case ProofResult.Yes(proofB, model) =>
                      ProofResult.Yes(Proof.ConjunctIntro(proofA, proofB), model)
                  }
            }

        case Or(a, b) =>
          val commuteOr =
            infer(Or(b, a), model, solveState2.consumeFuel)
              .map {
                case ProofResult.No(proof, model) =>
                  ProofResult.No(
                    Proof.DeMorganAndPullNotOut(Proof.ConjunctCommute(Proof.DeMorganOrPushNotIn(proof))),
                    model,
                  )

                case ProofResult.Yes(proof, model) =>
                  ProofResult.Yes(Proof.DisjunctCommute(proof), model)
              }

          val proveA = solve(a, model, solveState2.consumeFuel)
            .flatMap {
              case _: ProofResult.No => ZStream.empty
              case ProofResult.Yes(proof, model) =>
                ZStream.succeed(ProofResult.Yes(Proof.DisjunctIntroLeft(proof), model))
            }

          val proveB = solve(b, model, solveState2.consumeFuel)
            .flatMap {
              case _: ProofResult.No => ZStream.empty
              case ProofResult.Yes(proof, model) =>
                ZStream.succeed(ProofResult.Yes(Proof.DisjunctIntroRight(proof), model))
            }

          commuteOr ++ proveA ++ proveB

        case Implies(And(a, b), PropFalse) =>
          solve(Or(Implies(a, PropFalse), Implies(b, PropFalse)), model, solveState2.consumeFuel)
            .mapZIO {
              case ProofResult.No(proof, model) =>
                for
                  id <- UniqueIdentifier.make
                yield ProofResult.No(
                  Proof.HypotheticalSyllogism(
                    Proof.ImplicaitonAbstraction(id, Proof.DeMorganAndPushNotIn(Proof.Identifier(id))),
                    proof,
                  ),
                  model,
                )

              case ProofResult.Yes(proof, model) =>
                ZIO.succeed(ProofResult.Yes(Proof.DeMorganOrPullNotOut(proof), model))
            }

        case Implies(Or(a, b), PropFalse) =>
          solve(And(Implies(a, PropFalse), Implies(b, PropFalse)), model, solveState2.consumeFuel)
            .mapZIO {
              case ProofResult.No(notProof, model) =>
                for
                  id <- UniqueIdentifier.make
                yield ProofResult.No(
                  Proof.HypotheticalSyllogism(
                    Proof.ImplicaitonAbstraction(id, Proof.DeMorganOrPushNotIn(Proof.Identifier(id))),
                    notProof,
                  ),
                  model,
                )

              case ProofResult.Yes(proof, model) =>
                ZIO.succeed(ProofResult.Yes(Proof.DeMorganAndPullNotOut(proof), model))
            }

        case Implies(Implies(a, PropFalse), PropFalse) =>
          solve(a, model, solveState2.consumeFuel).map {
            case ProofResult.No(notProof, model) =>
              ProofResult.No(Proof.DoubleNegIntro(notProof), model)

            case ProofResult.Yes(proof, model) =>
              ProofResult.Yes(Proof.DoubleNegIntro(proof), model)
          }

        case Implies(_, PropFalse) => ZStream.empty

        case Implies(a, b) =>
          ZStream.unwrap(
            for
              aId <- UniqueIdentifier.make
            yield solve(b, model, solveState.addGiven(Proof.Identifier(aId), a))
              .flatMap {
                case ProofResult.Yes(proof, model) =>
                  ZStream.succeed(ProofResult.Yes(Proof.ImplicaitonAbstraction(aId, proof), model))

                case ProofResult.No(_, _) =>
                  ZStream.empty
              }
          )

        case PropFalse => ZStream.empty

        case PredicateExpression(e) =>
          intrinsicPredicate(e, model, solveState2.consumeFuel)
      })
    end if



  private def infer(goal: Predicate, model: Model, solveState: SolveState): ZStream[R, E, ProofResult.Definitive] =
    if solveState.fuelEmpty then
      ZStream.empty
    else
      ZStream.unwrap(
        for {
          kb <- ZIO.foreach(freshAssertions(model)) { makeAssertion => makeAssertion(newVariable) }
        } yield ZStream.fromIterable(kb ++ solveState.additionalGivens).flatMap { (proof, assertion) =>
          inferOne(goal, assertion, model, solveState)
            .map {
              case InferOneResult.No(notProofBuilder, model) =>
                ProofResult.No(notProofBuilder(proof), model)
                
              case InferOneResult.Yes(proofBuilder, model) =>
                ProofResult.Yes(proofBuilder(proof), model)
            }
        }
      )

  private enum InferOneResult {
    case Yes(proofBuilder: Proof[ProofAtom] => Proof[ProofAtom], model: Model)
    case No(notProofBuilder: Proof[ProofAtom] => Proof[ProofAtom], model: Model)
  }
  
  private def inferOne(goal: Predicate, assertion: Predicate, model: Model, solveState: SolveState): ZStream[R, E, InferOneResult] =
    goal match {
      case Implies(_, _) =>
        ZStream.empty

      case _ =>

        val unifyAssertion =
          unifyAsStream(goal, assertion, model, solveState).map { model =>
            InferOneResult.Yes(identity, model)
          }

        val assertionSpecific =
          assertion match {
            case Implies(PropFalse, _) => ZStream.empty

            case Implies(premise, PropFalse) =>
              unifyAsStream(goal, premise, model, solveState.consumeFuel)
                .map { model =>
                    // Since goal == premise and assertion = not premise, assertion = not goal
                    // p proves assertion so p proves not goal
                    InferOneResult.No(identity, model)
                }

            case Implies(premise, conclusion) =>
              inferOne(goal, conclusion, model, solveState).flatMap {
                case InferOneResult.No(notProofBuilder, model) =>
                  solve(premise, model, solveState.consumeFuel)
                    .flatMap {
                      case ProofResult.Yes(premiseProof, model) =>
                        ZStream.succeed(InferOneResult.No(proof => Proof.ModusPonens(notProofBuilder(proof), premiseProof), model))

                      case ProofResult.No(_, _) => ZStream.empty
                    }

                case InferOneResult.Yes(proofBuilder, model) =>
                  solve(premise, model, solveState.consumeFuel)
                    .flatMap {
                      case ProofResult.Yes(premiseProof, model) =>
                        ZStream.succeed(InferOneResult.Yes(proof => Proof.ModusPonens(proofBuilder(proof), premiseProof), model))

                      case ProofResult.No(_, _) => ZStream.empty
                    }
              }

            case _ => ZStream.empty
          }

        unifyAssertion ++ assertionSpecific
    }
  
  private def unifyAsStream(goal: Predicate, rule: Predicate, model: Model, solveState: SolveState): ZStream[R, E, Model] =
    ZStream.unwrap(
      for
        modelRef <- Ref.make(model)
        didUnify <- unify(goal, rule, modelRef, solveState)
        model <- modelRef.get
      yield if didUnify then ZStream.succeed(model) else ZStream.empty
    )
  
  private def unify(goal: Predicate, rule: Predicate, model: Ref[Model], solveState: SolveState): ZIO[R, E, Boolean] =
    if solveState.fuelEmpty then
      ZIO.succeed(false)
    else
      (goal, rule) match {
        case (PredicateExpression(e1), PredicateExpression(e2)) =>
          unifyPredicateExpression(e1, e2, model, solveState.fuel)

        case (And(left1, right1), And(left2, right2)) =>
          unify(left1, left2, model, solveState) && unify(right1, right2, model, solveState)

        case (Or(left1, right1), Or(left2, right2)) =>
          unify(left1, left2, model, solveState) && unify(right1, right2, model, solveState)

        case (Implies(left1, right1), Implies(left2, right2)) =>
          unify(left1, left2, model, solveState) && unify(right1, right2, model, solveState)

        case (PropFalse, PropFalse) => ZIO.succeed(true)

        case _ => ZIO.succeed(false)
      }

}

object PrologContext {
  type Aux[R, E, TSyntax <: ProverSyntax] = PrologContext[R, E] { val syntax: TSyntax }
}
