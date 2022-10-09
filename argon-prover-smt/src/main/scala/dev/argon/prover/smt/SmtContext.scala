package dev.argon.prover.smt

import dev.argon.prover.*
import dev.argon.util.{*, given}
import zio.*
import zio.stm.*

abstract class SmtContext[R, E] extends ProverContext[R, E] {
  import syntax.*

  protected def newVariable: ZIO[R, E, TVariable]
  protected def assumeResultProof: Proof[ProofAtom]


  enum Literal derives CanEqual {
    def pf: PredicateFunction

    case Atom(pf: PredicateFunction)
    case NotAtom(pf: PredicateFunction)
  }

  type CNF = List[List[Literal]]

  type LiteralPlus = Literal | Boolean

  type CNFPlus = List[List[LiteralPlus]]

  private enum NNF {
    case Lit(literal: LiteralPlus)
    case And(conjuncts: List[NNF])
    case Or(disjuncts: List[NNF])
  }

  final case class PredEquivalenceClass(predicates: Seq[PredicateFunction])
  final case class ExprEquivalenceClass(exprs: Seq[Expr])
  final case class ProverState
  (
    model: Model,
    fuel: Int,
    predEqClasses: Seq[PredEquivalenceClass],
    exprEqClasses: Seq[ExprEquivalenceClass],
  )


  private def getEqClassIndex(pred: PredicateFunction, state: ProverState): ZIO[R, E, (ProverState, Int)] =
    val index = state.predEqClasses.indexWhere { _.predicates.contains(pred) }
    if index >= 0 then
      ZIO.succeed((state, index))
    else
      normalizePredicateFunction(pred, state.model, state.fuel).map { predNorm =>
        val index = state.predEqClasses.indexWhere { _.predicates.contains(predNorm) }
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
  end getEqClassIndex



  private def negationNormalForm(p: Predicate): NNF =
    p match {
      case And(a, b) => NNF.And(List(negationNormalForm(a), negationNormalForm(b)))
      case Or(a, b) => NNF.Or(List(negationNormalForm(a), negationNormalForm(b)))
      case Implies(a, PropFalse) => elimNot(a)
      case Implies(a, b) =>
        NNF.Or(List(elimNot(a), negationNormalForm(b)))

      case PropFalse => NNF.Lit(false)
      case pf @ PredicateFunction(_, _) => NNF.Lit(Literal.Atom(pf))
    }

  private def elimNot(p: Predicate): NNF =
    p match {
      case And(a, b) => NNF.Or(List(elimNot(a), elimNot(b)))
      case Or(a, b) => NNF.And(List(elimNot(a), elimNot(b)))
      case Implies(a, PropFalse) => negationNormalForm(a)
      case Implies(a, b) => NNF.And(List(negationNormalForm(a), elimNot(b)))
      case PropFalse => NNF.Lit(true)
      case pf @ PredicateFunction(_, _) => NNF.Lit(Literal.NotAtom(pf))
    }

  private def conjunctiveNormalForm(p: NNF): CNFPlus =
    p match {
      case NNF.Lit(literal) =>
        List(List(literal))

      case NNF.And(conjuncts) =>
        conjuncts.flatMap(conjunctiveNormalForm)

      case NNF.Or(Nil) =>
        List(Nil)

      case NNF.Or(head :: Nil) =>
        conjunctiveNormalForm(head)

      case NNF.Or(NNF.And(conjuncts) :: tail) =>
        conjuncts.flatMap { conjunct =>
          conjunctiveNormalForm(NNF.Or(conjunct :: tail))
        }

      case NNF.Or(NNF.Or(disjuncts) :: tail) =>
        conjunctiveNormalForm(NNF.Or(disjuncts ++ tail))

      case NNF.Or(NNF.Lit(literal) :: tail) =>
        conjunctiveNormalForm(NNF.Or(tail))
          .map { disjuncts =>
            literal :: disjuncts
          }
    }

  private def simplify(p: CNFPlus): CNF =
    p.flatMap(simplifyDisjunct)

  // Returns None to indicate that the disjunct is true
  private def simplifyDisjunct(p: List[LiteralPlus]): Option[List[Literal]] =
    p match {
      case Nil => Some(Nil)
      case (_: true) :: _ => None
      case (_: false) :: tail => simplifyDisjunct(tail)
      case (literal: Literal) :: tail => simplifyDisjunct(tail).map { literal :: _ }
    }

  enum AtomPolarity derives CanEqual {
    case Both, True, False
  }

  private def determinePolarity(p: CNF, state: ProverState): ZIO[R, E, (ProverState, Map[Int, AtomPolarity])] =
    ZIO.foldLeft(p.flatten)((state, Map.empty[Int, AtomPolarity])) { case ((state, acc), literal) =>
      getEqClassIndex(literal.pf, state).map { (state, eqClass) =>
        val currentPolarity = acc.get(eqClass)
        val updated = (literal, currentPolarity) match {
          case (_, Some(AtomPolarity.Both)) => acc

          case (Literal.Atom(_), Some(AtomPolarity.True)) => acc
          case (Literal.Atom(_), Some(AtomPolarity.False)) => acc.updated(eqClass, AtomPolarity.Both)
          case (Literal.Atom(_), None) => acc.updated(eqClass, AtomPolarity.True)

          case (Literal.NotAtom(_), Some(AtomPolarity.False)) => acc
          case (Literal.NotAtom(_), Some(AtomPolarity.True)) => acc.updated(eqClass, AtomPolarity.Both)
          case (Literal.NotAtom(_), None) => acc.updated(eqClass, AtomPolarity.False)
        }

        (state, updated)
      }
    }


  end determinePolarity

  private def elimLiterals(p: CNF, state: ProverState): ZIO[R, E, (CNF, ProverState)] =
    for
      (state, polarity) <- determinePolarity(p, state)
      stateRef <- Ref.make(state)
      updated <- {
        def elimDisjuncts(p: List[Literal]): ZIO[R, E, Option[List[Literal]]] =
          p match {
            case Nil => ZIO.some(Nil)
            case head :: tail =>
              for
                state <- stateRef.get
                (state, eqClass) <- getEqClassIndex(head.pf, state)
                _ <- stateRef.set(state)
                result <- (polarity.get(eqClass), head) match {
                  case (Some(AtomPolarity.True), Literal.Atom(_)) | (Some(AtomPolarity.False), Literal.NotAtom(_)) =>
                    ZIO.none // Remove this conjunct because it is true

                  case (Some(AtomPolarity.False), Literal.Atom(_)) | (Some(AtomPolarity.True), Literal.NotAtom(_)) =>
                    elimDisjuncts(tail) // Remove this disjunct because it is false

                  case (None | Some(AtomPolarity.Both), _) => elimDisjuncts(tail).map { _.map { head :: _ } }
                }
              yield result
          }

        ZIO.foreach(p)(elimDisjuncts)
      }
      state <- stateRef.get
    yield (updated.flatten, state)

  private def unitClauses(p: CNF): List[(PredicateFunction, Boolean)] =
    p.flatMap {
      case Literal.Atom(pf) :: Nil => List((pf, true))
      case Literal.NotAtom(pf) :: Nil => List((pf, false))
      case _ => Nil
    }


  private def unitPropagation(p: CNF, state: ProverState): ZIO[R, E, (CNF, ProverState)] =
    ZIO.foldLeft(unitClauses(p))((p, state)) {
      case ((p, state), (pf, value)) =>
        getEqClassIndex(pf, state).flatMap { (state, eqClass) =>
          assumePredicate(p, state, eqClass, value)
        }
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

  private def preprocess(p: CNF, state: ProverState): ZIO[R, E, (CNF, ProverState)] =
    for
      (p, state) <- unitPropagation(p, state)
      (p, state) <- elimLiterals(p, state)
    yield (p, state)

  private final case class PredicateStats(numTrue: Int, numFalse: Int)

  private enum PredicateChoice {
    case KnownResult(result: Boolean)
    case SelectedPredicate(eqClass: Int, bestValue: Boolean)
  }

  private def choosePredicate(p: CNF, state: ProverState): ZIO[R, E, (ProverState, PredicateChoice)] =
    def impl(p: CNF, state: ProverState, stats: Map[Int, PredicateStats]): ZIO[R, E, (ProverState, PredicateChoice)] =
      p match {
        case Nil =>
          val bestPred = stats.maxByOption { (_, stat) => Math.max(stat.numTrue, stat.numFalse) }
          val result = bestPred match {
            case Some((eqClass, stats)) =>
              val bestValue = stats.numTrue >= stats.numFalse
              PredicateChoice.SelectedPredicate(eqClass, bestValue)

            case None =>
              PredicateChoice.KnownResult(true)
          }
          ZIO.succeed((state, result))

        case Nil :: _ => ZIO.succeed((state, PredicateChoice.KnownResult(false)))

        case (Literal.Atom(pf) :: ht) :: t =>
          getEqClassIndex(pf, state).flatMap { (state, eqClass) =>
            impl(ht :: t, state, stats.updatedWith(eqClass) {
              case None => Some(PredicateStats(numTrue = 1, numFalse = 0))
              case Some(stats) => Some(stats.copy(numTrue = stats.numTrue + 1))
            })
          }

        case (Literal.NotAtom(pf) :: ht) :: t =>
          getEqClassIndex(pf, state).flatMap { (state, eqClass) =>
            impl(ht :: t, state, stats.updatedWith(eqClass) {
              case None => Some(PredicateStats(numTrue = 0, numFalse = 1))
              case Some(stats) => Some(stats.copy(numFalse = stats.numFalse + 1))
            })
          }
      }

    impl(p, state, Map.empty)
  end choosePredicate



  private def satisfiable(p: CNF, state: ProverState): ZIO[R, E, Boolean] =
    preprocess(p, state).flatMap { (p, state) =>
      choosePredicate(p, state).flatMap {
        case (_, PredicateChoice.KnownResult(result)) => ZIO.succeed(result)
        case (state, PredicateChoice.SelectedPredicate(pf, value)) =>
          assumePredicate(p, state, pf, value).flatMap(satisfiable) ||
            assumePredicate(p, state, pf, !value).flatMap(satisfiable)
      }
    }

  final case class QuantifiedPredicate(vars: Set[TVariable], expr: Predicate)

  private def assertionAsQuantifier(assertion: ZIO[R, E, TVariable] => ZIO[R, E, (Proof[ProofAtom], Predicate)]): ZIO[R, E, QuantifiedPredicate] =
    for
      vars <- TSet.empty[TVariable].commit
      (_, pred) <- assertion(newVariable.tap(vars.put(_).commit))
      vars <- vars.toSet.commit
    yield QuantifiedPredicate(vars, pred)


  override def check(goal: Predicate, model: Model, fuel: Int): ZIO[R, E, ProofResult] =
    ZIO.foreach(assertions) { assertion =>
      assertionAsQuantifier(assertion)
    }
      .flatMap { quantAsserts =>
        val unquantAsserts = quantAsserts.collect {
          case QuantifiedPredicate(vars, expr) if vars.isEmpty => expr
        }

        val p = unquantAsserts.foldLeft(Implies(goal, PropFalse))(And.apply)


        val nnf = negationNormalForm(p)
        val cnfPlus = conjunctiveNormalForm(nnf)
        val cnf = simplify(cnfPlus)

        val initialState = ProverState(
          model = model,
          fuel = fuel,
          predEqClasses = Seq.empty,
          exprEqClasses = Seq.empty,
        )

        satisfiable(cnf, initialState).map {
          case true => ProofResult.Unknown
          case false => ProofResult.Yes(assumeResultProof, model)
        }
      }
  end check
}
