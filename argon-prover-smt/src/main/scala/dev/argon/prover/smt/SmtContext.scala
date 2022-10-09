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

  private def determinePolarity(p: CNF): Map[PredicateFunction, AtomPolarity] =
    p.iterator.flatten.foldLeft(Map.empty[PredicateFunction, AtomPolarity]) { (acc, literal) =>
      val currentPolarity = acc.get(literal.pf)
      (literal, currentPolarity) match {
        case (_, Some(AtomPolarity.Both)) => acc

        case (Literal.Atom(_), Some(AtomPolarity.True)) => acc
        case (Literal.Atom(pf), Some(AtomPolarity.False)) => acc.updated(pf, AtomPolarity.Both)
        case (Literal.Atom(pf), None) => acc.updated(pf, AtomPolarity.True)

        case (Literal.NotAtom(_), Some(AtomPolarity.False)) => acc
        case (Literal.NotAtom(pf), Some(AtomPolarity.True)) => acc.updated(pf, AtomPolarity.Both)
        case (Literal.NotAtom(pf), None) => acc.updated(pf, AtomPolarity.False)
      }
    }


  end determinePolarity

  private def elimLiterals(p: CNF): CNF =
    val polarity = determinePolarity(p)

    def elimDisjuncts(p: List[Literal]): Option[List[Literal]] =
      p match {
        case Nil => Some(Nil)
        case head :: tail =>
          polarity.get(head.pf) match {
            case Some(AtomPolarity.True) => None
            case Some(AtomPolarity.False) => elimDisjuncts(tail)
            case None | Some(AtomPolarity.Both) => elimDisjuncts(tail).map { head :: _ }
          }
      }

    p.flatMap(elimDisjuncts)
  end elimLiterals

  private def unitClauses(p: CNF): List[(PredicateFunction, Boolean)] =
    p.flatMap {
      case Literal.Atom(pf) :: Nil => List((pf, true))
      case Literal.NotAtom(pf) :: Nil => List((pf, false))
      case _ => Nil
    }


  private def unitPropagation(p: CNF, state: ProverState): ZIO[R, E, (CNF, ProverState)] =
    ZIO.foldLeft(unitClauses(p))((p, state)) {
      case ((p, state), (pf, value)) =>
        assumePredicate(p, state, pf, value)
    }



  protected def assumePredicate(p: CNF, state: ProverState, pf: PredicateFunction, value: Boolean): ZIO[R, E, (CNF, ProverState)] =
    for
      (state, eqClassIndex) <- getEqClassIndex(pf, state)
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
                if eqClassIndex == litEqClass then
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
      p2 = elimLiterals(p)
    yield (p2, state)


  private final case class PredicateStats(numTrue: Int, numFalse: Int)

  private enum PredicateChoice {
    case KnownResult(result: Boolean)
    case SelectedPredicate(pf: PredicateFunction, bestValue: Boolean)
  }

  private def choosePredicate(p: CNF): PredicateChoice =
    def impl(p: CNF, stats: Map[PredicateFunction, PredicateStats]): PredicateChoice =
      p match {
        case Nil =>
          val bestPred = stats.maxByOption { (pf, stat) => Math.max(stat.numTrue, stat.numFalse) }
          bestPred match {
            case Some((pf, stats)) =>
              val bestValue = stats.numTrue >= stats.numFalse
              PredicateChoice.SelectedPredicate(pf, bestValue)

            case None =>
              PredicateChoice.KnownResult(true)
          }

        case Nil :: _ => PredicateChoice.KnownResult(false)
        case (Literal.Atom(pf) :: ht) :: t =>
          impl(ht :: t, stats.updatedWith(pf) {
            case None => Some(PredicateStats(numTrue = 1, numFalse = 0))
            case Some(stats) => Some(stats.copy(numTrue = stats.numTrue + 1))
          })

        case (Literal.NotAtom(pf) :: ht) :: t =>
          impl(ht :: t, stats.updatedWith(pf) {
            case None => Some(PredicateStats(numTrue = 0, numFalse = 1))
            case Some(stats) => Some(stats.copy(numFalse = stats.numFalse + 1))
          })
      }

    impl(p, Map.empty)
  end choosePredicate



  private def satisfiable(p: CNF, state: ProverState): ZIO[R, E, Boolean] =
    preprocess(p, state).flatMap { (p, state) =>
      choosePredicate(p) match {
        case PredicateChoice.KnownResult(result) => ZIO.succeed(result)
        case PredicateChoice.SelectedPredicate(pf, value) =>
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
