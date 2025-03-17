package dev.argon.prover.smt

import dev.argon.prover.*
import dev.argon.util.{*, given}
import zio.*
import zio.stm.*

import scala.reflect.TypeTest

abstract class SmtContext[R, E](using TypeTest[Any, E]) extends ProverContext[R, E] {
  import syntax.*

  protected def assumeResultProof: Proof[ProofAtom]

  protected def normalizePredicateExpression(p: TPredicateExpr, model: Model, fuel: Fuel): ZIO[R, E, TPredicateExpr]


  enum Literal derives CanEqual {
    def pf: TPredicateExpr

    case Atom(pf: TPredicateExpr)
    case NotAtom(pf: TPredicateExpr)
  }

  type CNF = List[List[Literal]]

  type LiteralPlus = Literal | Boolean

  type CNFPlus = List[List[LiteralPlus]]

  private enum NNF {
    case Lit(literal: LiteralPlus)
    case And(conjuncts: List[NNF])
    case Or(disjuncts: List[NNF])
  }

  final case class PredEquivalenceClass(predicates: Seq[TPredicateExpr])
  final case class QuantifiedPredicate(vars: Set[TVariable], trigger: Seq[Literal], expr: CNF)
  final case class KnownPredicate(p: TPredicateExpr, value: Boolean)
  final case class ProverState
  (
    model: Model,
    fuel: Fuel,
    predEqClasses: Seq[PredEquivalenceClass],
    quantAsserts: Seq[QuantifiedPredicate],
    instantiatedEqClasses: Set[Int],
    knownPredicates: Seq[KnownPredicate],
    additionalConstraints: CNF,
  ) {
    def consumeFuel: ProverState = copy(fuel = fuel.consume)
  }

  final case class SmtConstraint(p: Predicate)


  final case class TheoryResponse(
    impliedAssignments: Seq[KnownPredicate],
    constraints: Predicate,
  )

  trait Theory {
    def check(assigned: Seq[KnownPredicate], unassigned: Seq[TPredicateExpr], model: Model, fuel: Fuel): ZIO[R, E | SmtConstraint, Seq[KnownPredicate]]
  }

  protected val theories: Seq[Theory]

  protected def predicateReferencesVariable(p: TPredicateExpr, v: TVariable): Boolean


  private def predicatesEquivalent(p1: TPredicateExpr, p2: TPredicateExpr, state: ProverState): ZIO[R, E, Boolean] =
    matchPredicateExpr(p1, p2, state, Set.empty).map(_.nonEmpty)

  private def findMatchingIndex[A](c: Iterable[A])(f: A => ZIO[R, E, Boolean]): ZIO[R, E, Int] =
    ZIO.collectFirst(c.zipWithIndex) { case (a, i) =>
      f(a).map {
        case true => Some(i)
        case false => None
      }
    }.map { _.getOrElse(-1) }

  private def getEqClassIndex(pred: TPredicateExpr, state: ProverState): ZIO[R, E, (ProverState, Int)] =
    findMatchingIndex(state.predEqClasses) { predEqClass => ZIO.exists(predEqClass.predicates)(predicatesEquivalent(pred, _, state)) }
      .flatMap { index =>
        if index >= 0 then
          ZIO.succeed((state, index))
        else
          normalizePredicateExpression(pred, state.model, state.fuel).map { predNorm =>
            val index = state.predEqClasses.indexWhere {
              _.predicates.contains(predNorm)
            }
            if index >= 0 then
              val eqClass = state.predEqClasses(index)
              val newState = state.copy(
                predEqClasses = state.predEqClasses.updated(index, PredEquivalenceClass(eqClass.predicates :+ pred))
              )
              (newState, index)
            else
              val newState = state.copy(
                predEqClasses = state.predEqClasses :+ PredEquivalenceClass(Seq(pred, predNorm))
              )
              (newState, state.predEqClasses.size)
            end if
          }
      }

  private def recordKnownPredicate(state: ProverState, pf: TPredicateExpr, value: Boolean): ZIO[R, E, ProverState] =
    ZIO.succeed { state.copy(knownPredicates = state.knownPredicates :+ KnownPredicate(pf, value)) }


  private def negationNormalForm(p: Predicate): NNF =
    p match {
      case And(a, b) => NNF.And(List(negationNormalForm(a), negationNormalForm(b)))
      case Or(a, b) => NNF.Or(List(negationNormalForm(a), negationNormalForm(b)))
      case Implies(a, PropFalse) => elimNot(a)
      case Implies(a, b) =>
        NNF.Or(List(elimNot(a), negationNormalForm(b)))

      case PropTrue => NNF.Lit(true)
      case PropFalse => NNF.Lit(false)
      case PredicateExpression(e) => NNF.Lit(Literal.Atom(e))
    }

  private def elimNot(p: Predicate): NNF =
    p match {
      case And(a, b) => NNF.Or(List(elimNot(a), elimNot(b)))
      case Or(a, b) => NNF.And(List(elimNot(a), elimNot(b)))
      case Implies(a, PropFalse) => negationNormalForm(a)
      case Implies(a, b) => NNF.And(List(negationNormalForm(a), elimNot(b)))
      case PropTrue => NNF.Lit(false)
      case PropFalse => NNF.Lit(true)
      case PredicateExpression(e) => NNF.Lit(Literal.NotAtom(e))
    }

  private def conjunctiveNormalFormUnsimplified(p: NNF): CNFPlus =
    p match {
      case NNF.Lit(literal) =>
        List(List(literal))

      case NNF.And(conjuncts) =>
        conjuncts.flatMap(conjunctiveNormalFormUnsimplified)

      case NNF.Or(Nil) =>
        List(Nil)

      case NNF.Or(head :: Nil) =>
        conjunctiveNormalFormUnsimplified(head)

      case NNF.Or(NNF.And(conjuncts) :: tail) =>
        conjuncts.flatMap { conjunct =>
          conjunctiveNormalFormUnsimplified(NNF.Or(conjunct :: tail))
        }

      case NNF.Or(NNF.Or(disjuncts) :: tail) =>
        conjunctiveNormalFormUnsimplified(NNF.Or(disjuncts ++ tail))

      case NNF.Or(NNF.Lit(literal) :: tail) =>
        conjunctiveNormalFormUnsimplified(NNF.Or(tail))
          .map { disjuncts =>
            literal :: disjuncts
          }
    }

  private def simplify(p: CNFPlus): CNF =
    p.flatMap(simplifyDisjunct)


  private def conjunctiveNormalForm(p: Predicate): CNF =
    simplify(conjunctiveNormalFormUnsimplified(negationNormalForm(p)))

  // Returns None to indicate that the disjunct is true
  private def simplifyDisjunct(p: List[LiteralPlus]): Option[List[Literal]] =
    p match {
      case Nil => Some(Nil)
      case (_: true) :: _ => None
      case (_: false) :: tail => simplifyDisjunct(tail)
      case (literal: Literal) :: tail => simplifyDisjunct(tail).map { literal :: _ }
    }

  protected def substituteVariablesPE(varMap: Map[TVariable, TPredicateExpr])(pf: TPredicateExpr): TPredicateExpr

  private def substituteVariablesCNF(varMap: Map[TVariable, TPredicateExpr])(p: CNF): CNF =
    p.map { _.map {
      case Literal.Atom(pf) => Literal.Atom(substituteVariablesPE(varMap)(pf))
      case Literal.NotAtom(pf) => Literal.NotAtom(substituteVariablesPE(varMap)(pf))
    } }

  enum AtomPolarity derives CanEqual {
    case Both
    case True(example: TPredicateExpr)
    case False(example: TPredicateExpr)
  }

  private def unitClauses(p: CNF): List[KnownPredicate] =
    p.flatMap {
      case Literal.Atom(pf) :: Nil => List(KnownPredicate(pf, true))
      case Literal.NotAtom(pf) :: Nil => List(KnownPredicate(pf, false))
      case _ => Nil
    }

  protected def assumePredicate(p: CNF, state: ProverState, eqClass: Int, value: Boolean): ZIO[R, E, (CNF, ProverState)] =
    for
      stateRef <- Ref.make(state)

      updated <- {
        def impl(p: CNF): ZIO[R, E, CNFPlus] =
          ZIO.foreach(p) { disjuncts =>
            ZIO.foreach(disjuncts) { lit =>
              for
                state <- stateRef.get
                (state, litEqClass) <- getEqClassIndex(lit.pf, state)
                _ <- stateRef.set(state)
              yield
                if eqClass == litEqClass then
                  lit match {
                    case Literal.Atom(_) => value
                    case Literal.NotAtom(_) => !value
                  }
                else
                  lit
            }
          }

        impl(p)
      }

      state <- stateRef.get
    yield (simplify(updated), state)

  protected def assumeKnownPredicates(p: CNF, state: ProverState): ZIO[R, E, (CNF, ProverState)] =
    ZIO.foldLeft(state.knownPredicates)((p, state)) { case ((p, state), KnownPredicate(pf, value)) =>
      getEqClassIndex(pf, state).flatMap { case (state, eqClass) =>
        assumePredicate(p, state, eqClass, value)
      }
    }

  private def processKnownValues(p: CNF, state: ProverState): ZIO[R, E, (CNF, ProverState)] =
    assumeKnownPredicates(p, state).flatMap { (p, state) =>
      val units = unitClauses(p)
      if units.isEmpty then
        ZIO.succeed((p, state))
      else
        processKnownValues(p, state.copy(knownPredicates = state.knownPredicates ++ units))
    }

  private def checkModel(p: CNF, state: ProverState): ZIO[R, E | SmtConstraint, (CNF, ProverState)] =
    assumeKnownPredicates(p, state).flatMap { (p, state) =>
      ZIO.foldLeft(theories)((p, state)) { case ((p, state), theory) =>
        val unassigned = p.view.flatten.map(_.pf).toSet.toSeq
        theory.check(state.knownPredicates, unassigned, state.model, state.fuel)
          .flatMap { impliedAssignments =>
            if impliedAssignments.isEmpty then
              ZIO.succeed((p, state))
            else
              processKnownValues(p, state.copy(knownPredicates = state.knownPredicates ++ impliedAssignments))
          }
      }
    }

  private final case class PredicateStats(pf: TPredicateExpr, numTrue: Int, numFalse: Int)

  private enum PredicateChoice {
    case KnownResult(result: Boolean)
    case SelectedPredicate(p: TPredicateExpr, eqClass: Int, bestValue: Boolean)
  }

  private def choosePredicate(p: CNF, state: ProverState): ZIO[R, E, (ProverState, PredicateChoice)] =
    def impl(p: CNF, state: ProverState, stats: Map[Int, PredicateStats]): ZIO[R, E, (ProverState, PredicateChoice)] =
      p match {
        case Nil =>
          val bestPred = stats.maxByOption { (_, stat) => Math.max(stat.numTrue, stat.numFalse) }
          val result = bestPred match {
            case Some((eqClass, stats)) =>
              val bestValue = stats.numTrue >= stats.numFalse
              PredicateChoice.SelectedPredicate(stats.pf, eqClass, bestValue)

            case None =>
              PredicateChoice.KnownResult(true)
          }
          ZIO.succeed((state, result))

        case Nil :: _ => ZIO.succeed((state, PredicateChoice.KnownResult(false)))

        case (Literal.Atom(pf) :: ht) :: t =>
          val next = if ht.isEmpty then t else ht :: t
          getEqClassIndex(pf, state).flatMap { (state, eqClass) =>
            impl(next, state, stats.updatedWith(eqClass) {
              case None => Some(PredicateStats(pf, numTrue = 1, numFalse = 0))
              case Some(stats) => Some(stats.copy(numTrue = stats.numTrue + 1))
            })
          }

        case (Literal.NotAtom(pf) :: ht) :: t =>
          val next = if ht.isEmpty then t else ht :: t
          getEqClassIndex(pf, state).flatMap { (state, eqClass) =>
            impl(next, state, stats.updatedWith(eqClass) {
              case None => Some(PredicateStats(pf, numTrue = 0, numFalse = 1))
              case Some(stats) => Some(stats.copy(numFalse = stats.numFalse + 1))
            })
          }
      }

    impl(p, state, Map.empty)
  end choosePredicate

  protected def matchPredicateExpr(pf: TPredicateExpr, quantPF: TPredicateExpr, state: ProverState, quantVars: Set[TVariable]): ZIO[R, E, Option[Map[TVariable, TPredicateExpr]]]

  private def quantLitMatch(state: ProverState, lit: Literal, quantLit: Literal, quantVars: Set[TVariable]): ZIO[R, E, Option[Map[TVariable, TPredicateExpr]]] =
    (lit, quantLit) match {
      case (Literal.Atom(pf), Literal.NotAtom(quantPF)) => matchPredicateExpr(pf, quantPF, state, quantVars)
      case (Literal.NotAtom(pf), Literal.Atom(quantPF)) => matchPredicateExpr(pf, quantPF, state, quantVars)
      case _ => ZIO.none
    }


  private def instantiateQuantifier(lit: Literal, state: ProverState): ZIO[R, E, (ProverState, CNF)] =
    def quantMatch(lit: Literal)(quant: QuantifiedPredicate): ZIO[R, E, Option[CNF]] =
      ZIO.collectFirst(quant.trigger) { quantLit =>
        quantLitMatch(state, lit, quantLit, quant.vars).flatMap {
          case Some(varMap) =>
            for
              varMap <- ZIO.foreach(quant.vars.toSeq) { quantVar =>
                varMap.get(quantVar) match {
                  case Some(value) => ZIO.succeed(quantVar -> value)
                  case None => newVariable.map { newVar => quantVar -> variableToExpr(newVar) }
                }
              }.map { _.toMap }

            yield Some(substituteVariablesCNF(varMap)(quant.expr))

          case None =>
            ZIO.none
        }
      }

    getEqClassIndex(lit.pf, state).flatMap { case (state, eqClass) =>
      if state.instantiatedEqClasses.contains(eqClass) then
        ZIO.succeed((state, Nil))
      else
        for
          instantiated <- ZIO.foreach(state.quantAsserts)(quantMatch(lit))
          instantiatedFlat = instantiated.iterator.flatten.flatten.toList
        yield (
          state.copy(instantiatedEqClasses = state.instantiatedEqClasses + eqClass),
          instantiatedFlat
        )
    }
  end instantiateQuantifier

  private def instantiateQuantifiers(p: CNF, state: ProverState): ZIO[R, E, (ProverState, CNF)] =
    if state.fuel.nonEmpty then
      for
        stateRef <- Ref.make(state)
        clauses <- ZIO.foreach(p.flatten) { lit =>
          stateRef.get
            .flatMap { state =>
              instantiateQuantifier(lit, state)
            }
            .flatMap {
              case (state, clauses) =>
                stateRef.set(state).as(clauses)
            }
        }
        state <- stateRef.get
        flatClauses = clauses.flatten
      yield (
        if flatClauses.isEmpty then state else state.consumeFuel,
        flatClauses
      )
    else
      ZIO.succeed((state, Nil))

  private enum SatResult derives CanEqual {
    case Unknown
    case Sat(model: Model)
    case Unsat(conflicts: CNF)
  }

  private def satisfiable(p: CNF, state: ProverState): ZIO[R, E, SatResult] =
    def satNoQuant(p: CNF, state: ProverState) =
      checkModel(p, state).foldCauseZIO(
        failure = cause => ErrorTestUtils.splitCause[SmtConstraint, E](cause) match {
          case Left(SmtConstraint(constraint)) =>
            ZIO.succeed(SatResult.Unsat(state.additionalConstraints ++ conjunctiveNormalForm(constraint)))

          case Right(cause) => ZIO.failCause(cause)
        },
        success = (p, state) => {
          choosePredicate(p, state).flatMap {
            case (_, PredicateChoice.KnownResult(true)) => ZIO.succeed(SatResult.Sat(state.model))
            case (state, PredicateChoice.KnownResult(false)) => ZIO.succeed(SatResult.Unsat(state.additionalConstraints))

            case (state, PredicateChoice.SelectedPredicate(pred, eqClass, value)) =>
              assumeKnownPredicates(p, state.copy(knownPredicates = state.knownPredicates :+ KnownPredicate(pred, value)))
                .flatMap { (p, state1) =>
                  satisfiable(p, state1)
                }
                .flatMap {
                  case SatResult.Unknown => ZIO.succeed(SatResult.Unknown)
                  case sat @ SatResult.Sat(_) => ZIO.succeed(sat)
                  case SatResult.Unsat(constraint) =>
                    assumeKnownPredicates(p, state.copy(knownPredicates = state.knownPredicates :+ KnownPredicate(pred, !value)))
                      .flatMap { (p, state2) =>
                        assumeKnownPredicates(
                          p ++ constraint,
                          state.copy(
                            knownPredicates = state.knownPredicates :+ KnownPredicate(pred, !value),
                            additionalConstraints = constraint
                          )
                        )
                          .flatMap { (p, state) =>
                            satisfiable(p, state)
                          }
                      }
                }
          }
        },
      )

    if state.fuel.nonEmpty then
      val oldFuel = state.fuel
      instantiateQuantifiers(p, state).flatMap { (state, clauses) =>
        satNoQuant(p ++ clauses, if clauses.nonEmpty then state.copy(fuel = oldFuel.consume) else state)
      }
    else
      satNoQuant(p, state)
    end if
  end satisfiable

  private def getAssertionTrigger(p: Predicate, vars: Set[TVariable], invert: Boolean): Set[Literal] =
    p match {
      case PredicateExpression(p) =>
        if !vars.exists(predicateReferencesVariable(p, _)) then Set()
        else if invert then Set(Literal.NotAtom(p))
        else Set(Literal.Atom(p))
      case And(a, b) => getAssertionTrigger(a, vars, invert) ++ getAssertionTrigger(b, vars, invert)
      case Or(a, b) => getAssertionTrigger(a, vars, invert) ++ getAssertionTrigger(b, vars, invert)

      case Implies(a, b) => getAssertionTrigger(a, vars, !invert) ++ getAssertionTrigger(b, vars, invert)

      case PropTrue | PropFalse => Set()
    }

  private def assertionAsQuantifier(assertion: ZIO[R, E, TVariable] => ZIO[R, E, (Proof[ProofAtom], Predicate)]): ZIO[R, E, QuantifiedPredicate] =
    for
      vars <- TSet.empty[TVariable].commit
      (_, pred) <- assertion(newVariable.tap(vars.put(_).commit))
      vars <- vars.toSet.commit
    yield QuantifiedPredicate(vars, getAssertionTrigger(pred, vars, false).toSeq, conjunctiveNormalForm(pred))


  override def check(goal: Predicate, model: Model, fuel: Fuel): ZIO[R, E, ProofResult] =
    ZIO.foreach(freshAssertions(model)) { assertion =>
      assertionAsQuantifier(assertion)
    }
      .flatMap { asserts =>
        
        val (unquantAsserts, quantAsserts) = asserts.partitionMap {
          case QuantifiedPredicate(vars, _, expr) if vars.isEmpty => Left(expr)
          case qp => Right(qp)
        }
        
        val p = unquantAsserts.iterator.flatten.toList ++ conjunctiveNormalForm(Implies(goal, PropFalse))

        val initialState = ProverState(
          model = model,
          fuel = fuel,
          predEqClasses = Seq.empty,
          quantAsserts = quantAsserts,
          instantiatedEqClasses = Set.empty,
          knownPredicates = Seq.empty,
          additionalConstraints = Nil,
        )

        satisfiable(p, initialState).map {
          case SatResult.Unknown | SatResult.Sat(_) => ProofResult.Unknown
          case SatResult.Unsat(_) => ProofResult.Yes(assumeResultProof, model)
        }
      }
  end check
}
