package dev.argon.prover.smt

import dev.argon.prover.*
import dev.argon.util.{*, given}

abstract class SmtContext[R, E] extends ProverContext[R, E] {
  import syntax.*

  private enum Literal derives CanEqual {
    def pf: PredicateFunction

    case Atom(pf: PredicateFunction)
    case NotAtom(pf: PredicateFunction)
  }

  private type CNF = List[List[Literal]]

  private enum LiteralPlus derives CanEqual {
    case True, False
    case Atom(pf: PredicateFunction)
    case NotAtom(pf: PredicateFunction)
  }

  private type CNFPlus = List[List[LiteralPlus]]

  private enum NNF {
    case Lit(literal: LiteralPlus)
    case And(conjuncts: List[NNF])
    case Or(disjuncts: List[NNF])
  }

  private def negationNormalForm(p: Predicate): NNF =
    p match {
      case And(a, b) => NNF.And(List(negationNormalForm(a), negationNormalForm(b)))
      case Or(a, b) => NNF.Or(List(negationNormalForm(a), negationNormalForm(b)))
      case Implies(a, PropFalse) => elimNot(a)
      case Implies(a, b) =>
        NNF.Or(List(elimNot(a), negationNormalForm(b)))

      case PropFalse => NNF.Lit(LiteralPlus.False)
      case pf @ PredicateFunction(_, _) => NNF.Lit(LiteralPlus.Atom(pf))
    }

  private def elimNot(p: Predicate): NNF =
    p match {
      case And(a, b) => NNF.Or(List(elimNot(a), elimNot(b)))
      case Or(a, b) => NNF.And(List(elimNot(a), elimNot(b)))
      case Implies(a, PropFalse) => negationNormalForm(a)
      case Implies(a, b) => NNF.And(List(negationNormalForm(a), elimNot(b)))
      case PropFalse => NNF.Lit(LiteralPlus.True)
      case pf @ PredicateFunction(_, _) => NNF.Lit(LiteralPlus.NotAtom(pf))
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
      case LiteralPlus.True :: _ => None
      case LiteralPlus.False :: tail => simplifyDisjunct(tail)
      case LiteralPlus.Atom(pf) :: tail => simplifyDisjunct(tail).map { Literal.Atom(pf) :: _ }
      case LiteralPlus.NotAtom(pf) :: tail => simplifyDisjunct(tail).map { Literal.NotAtom(pf) :: _ }
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

  private def unitPropagation(p: CNF): CNF =
    

}
