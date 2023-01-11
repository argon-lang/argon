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
  final case class QuantifiedPredicate(vars: Set[TVariable], trigger: CNF, expr: CNF)
  final case class ProverState
  (
    model: Model,
    fuel: Int,
    predEqClasses: Seq[PredEquivalenceClass],
    exprEqClasses: Seq[ExprEquivalenceClass],
    quantAsserts: Seq[QuantifiedPredicate],
    instantiatedEqClasses: Set[Int],
    knownPredicates: Seq[(PredicateFunction, Boolean)],
  ) {
    def consumeFuel: ProverState = copy(fuel = fuel - 1)
  }

  protected def predicateSatisfiableInModel(pf: PredicateFunction, state: ProverState): ZIO[R, E, Option[Boolean]] =
    ZIO.none

  protected def predicatesEquivalent(p1: PredicateFunction, p2: PredicateFunction, state: ProverState): ZIO[R, E, Boolean] =
    ZIO.succeed { p1 == p2 }

  private def findMatchingIndex[A](c: Iterable[A])(f: A => ZIO[R, E, Boolean]): ZIO[R, E, Int] =
    ZIO.collectFirst(c.zipWithIndex) { case (a, i) =>
      f(a).map {
        case true => Some(i)
        case false => None
      }
    }.map { _.getOrElse(-1) }

  private def getEqClassIndex(pred: PredicateFunction, state: ProverState): ZIO[R, E, (ProverState, Int)] =
    findMatchingIndex(state.predEqClasses) { predEqClass => ZIO.exists(predEqClass.predicates)(predicatesEquivalent(pred, _, state)) }
      .flatMap { index =>
        if index >= 0 then
          ZIO.succeed((state, index))
        else
          normalizePredicateFunction(pred, state.model, state.fuel).map { predNorm =>
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

  private def recordKnownPredicate(state: ProverState, pf: PredicateFunction, value: Boolean): ZIO[R, E, ProverState] =
    ZIO.succeed { state.copy(knownPredicates = state.knownPredicates :+ (pf, value)) }


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

  private def substituteVariables(varMap: Map[TVariable, Expr], expr: Expr): Expr =
    expr match {
      case Variable(v) =>
        varMap.get(v) match {
          case Some(value) => value
          case None => expr
        }

      case Value(ctor, args) =>
        Value(ctor, args.map(substituteVariables(varMap, _)))
    }

  private def substituteVariablesPF(varMap: Map[TVariable, Expr], pf: PredicateFunction): PredicateFunction =
    PredicateFunction(pf.function, pf.args.map { arg => substituteVariables(varMap, arg) })

  private def substituteVariablesCNF(varMap: Map[TVariable, Expr], p: CNF): CNF =
    p.map { _.map {
      case Literal.Atom(pf) => Literal.Atom(substituteVariablesPF(varMap, pf))
      case Literal.NotAtom(pf) => Literal.NotAtom(substituteVariablesPF(varMap, pf))
    } }

  enum AtomPolarity derives CanEqual {
    case Both
    case True(example: PredicateFunction)
    case False(example: PredicateFunction)
  }

  private def unitClauses(p: CNF): List[(PredicateFunction, Boolean)] =
    p.flatMap {
      case Literal.Atom(pf) :: Nil => List((pf, true))
      case Literal.NotAtom(pf) :: Nil => List((pf, false))
      case _ => Nil
    }


  private def unitPropagation(p: CNF, state: ProverState): ZIO[R, E, ProverState] =
    ZIO.foldLeft(unitClauses(p))(state) {
      case (state, (pf, value)) =>
        recordKnownPredicate(state, pf, value)
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
    ZIO.foldLeft(state.knownPredicates)((p, state)) { case ((p, state), (pf, value)) =>
      getEqClassIndex(pf, state).flatMap { case (state, eqClass) =>
        assumePredicate(p, state, eqClass, value)
      }
    }

  private def checkModel(p: CNF, state: ProverState): ZIO[R, E, ProverState] =
    ZIO.foldLeft(p.flatten)(state) { (state, lit) =>
      predicateSatisfiableInModel(lit.pf, state).flatMap {
        case Some(value) => recordKnownPredicate(state, lit.pf, value)
        case None => ZIO.succeed(state)
      }
    }

  private def preprocess(p: CNF, state: ProverState): ZIO[R, E, (CNF, ProverState)] =
    for
      (p, state) <- assumeKnownPredicates(p, state)
      state <- checkModel(p, state)
      (p, state) <- assumeKnownPredicates(p, state)
      state <- unitPropagation(p, state)
      (p, state) <- assumeKnownPredicates(p, state)
    yield (p, state)

  private final case class PredicateStats(pf: PredicateFunction, numTrue: Int, numFalse: Int)

  private enum PredicateChoice {
    case KnownResult(result: Boolean)
    case SelectedPredicate(lit: Literal, eqClass: Int, bestValue: Boolean)
  }

  private def choosePredicate(p: CNF, state: ProverState): ZIO[R, E, (ProverState, PredicateChoice)] =
    def impl(p: CNF, state: ProverState, stats: Map[Int, PredicateStats]): ZIO[R, E, (ProverState, PredicateChoice)] =
      p match {
        case Nil =>
          val bestPred = stats.maxByOption { (_, stat) => Math.max(stat.numTrue, stat.numFalse) }
          val result = bestPred match {
            case Some((eqClass, stats)) =>
              val bestValue = stats.numTrue >= stats.numFalse
              val lit = if bestValue then Literal.Atom(stats.pf) else Literal.NotAtom(stats.pf)
              PredicateChoice.SelectedPredicate(lit, eqClass, bestValue)

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

  protected def matchExpr(state: ProverState, expr: Expr, quantExpr: Expr, quantVars: Set[TVariable], acc: Map[TVariable, Expr]): ZIO[R, Option[E], Map[TVariable, Expr]] =
    (expr, quantExpr) match {
      case (_, Variable(v)) if quantVars.contains(v) =>
        acc.get(v) match {
          case Some(mappedExpr) =>
            if expr == mappedExpr then
              ZIO.succeed(acc)
            else
              ZIO.fail(None)

          case None =>
            ZIO.succeed(acc.updated(v, expr))
        }

      case (Variable(v1), Variable(v2)) if v1 == v2 =>
        ZIO.succeed(acc)

      case (Value(ctor1, args1), Value(ctor2, args2)) if ctor1 == ctor2 && args1.size == args2.size =>
        ZIO.foldLeft(args1.zip(args2))(acc) { case (acc, (e1, e2)) => matchExpr(state, e1, e2, quantVars, acc) }

      case _ => ZIO.fail(None)
    }

  protected def matchPredicateFunction(state: ProverState, pf: PredicateFunction, quantPF: PredicateFunction, quantVars: Set[TVariable], acc: Map[TVariable, Expr]): ZIO[R, Option[E], Map[TVariable, Expr]] =
    if pf.function == quantPF.function && pf.args.size == quantPF.args.size then
      ZIO.foldLeft(pf.args.zip(quantPF.args))(acc) { case (acc, (expr, quantExpr)) => matchExpr(state, expr, quantExpr, quantVars, acc) }
    else
      ZIO.fail(None)

  private def quantLitMatch(state: ProverState, lit: Literal, quantLit: Literal, quantVars: Set[TVariable]): ZIO[R, E, Option[Map[TVariable, Expr]]] =
    (lit, quantLit) match {
      case (Literal.Atom(pf), Literal.NotAtom(quantPF)) => matchPredicateFunction(state, pf, quantPF, quantVars, Map.empty).unsome
      case (Literal.NotAtom(pf), Literal.Atom(quantPF)) => matchPredicateFunction(state, pf, quantPF, quantVars, Map.empty).unsome
      case _ => ZIO.none
    }


  private def instantiateQuantifier(lit: Literal, state: ProverState): ZIO[R, E, (ProverState, CNF)] =
    def quantMatch(lit: Literal)(quant: QuantifiedPredicate): ZIO[R, E, Option[CNF]] =
      ZIO.collectFirst(quant.trigger.flatten) { quantLit =>
        quantLitMatch(state, lit, quantLit, quant.vars).flatMap {
          case Some(varMap) =>
            for
              varMap <- ZIO.foreach(quant.vars.toSeq) { quantVar =>
                varMap.get(quantVar) match {
                  case Some(value) => ZIO.succeed(quantVar -> value)
                  case None => newVariable.map { newVar => quantVar -> Variable(newVar) }
                }
              }.map { _.toMap }

            yield Some(substituteVariablesCNF(varMap, quant.expr))

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
    if state.fuel > 0 then
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

  private def satisfiable(p: CNF, state: ProverState): ZIO[R, E, Boolean] =
    instantiateQuantifiers(p, state).flatMap { (state, clauses) =>
      preprocess(p ++ clauses, state).flatMap { (p, state) =>
        choosePredicate(p, state).flatMap {
          case (_, PredicateChoice.KnownResult(result)) => ZIO.succeed(result)
          case (state, PredicateChoice.SelectedPredicate(_, _, _)) if clauses.nonEmpty =>
            satisfiable(p, state)

          case (state, PredicateChoice.SelectedPredicate(lit, eqClass, value)) if state.fuel > 0 =>
            recordKnownPredicate(state.consumeFuel, lit.pf, value).flatMap(assumePredicate(p, _, eqClass, value)).flatMap(satisfiable) ||
              recordKnownPredicate(state.consumeFuel, lit.pf, !value).flatMap(assumePredicate(p, _, eqClass, !value)).flatMap(satisfiable)

          case _ => ZIO.succeed(true)
        }
      }
    }

  private def getAssertionTrigger(p: Predicate): Predicate =
    p match {
      case Implies(_, PropFalse) => p
      case Implies(_, b) => getAssertionTrigger(b)
      case _ => p
    }

  private def assertionAsQuantifier(assertion: ZIO[R, E, TVariable] => ZIO[R, E, (Proof[ProofAtom], Predicate)]): ZIO[R, E, QuantifiedPredicate] =
    for
      vars <- TSet.empty[TVariable].commit
      (_, pred) <- assertion(newVariable.tap(vars.put(_).commit))
      vars <- vars.toSet.commit
    yield QuantifiedPredicate(vars, conjunctiveNormalForm(getAssertionTrigger(pred)), conjunctiveNormalForm(pred))


  override def check(goal: Predicate, model: Model, fuel: Int): ZIO[R, E, ProofResult] =
    ZIO.foreach(assertions) { assertion =>
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
          exprEqClasses = Seq.empty,
          quantAsserts = quantAsserts,
          instantiatedEqClasses = Set.empty,
          knownPredicates = Seq.empty,
        )

        satisfiable(p, initialState).map {
          case true => ProofResult.Unknown
          case false => ProofResult.Yes(assumeResultProof, model)
        }
      }
  end check
}
