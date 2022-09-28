package dev.argon.prover

import dev.argon.util.UniqueIdentifier

sealed trait Proof[+A] derives CanEqual

object Proof {
  final case class Atomic[+A](atom: A) extends Proof[A]

  // Reference an indentifier bound by a ImplicationAbstraction
  final case class Identifier(id: UniqueIdentifier) extends Proof[Nothing]

  // proves A -> B given an expression of B in terms of A
  final case class ImplicaitonAbstraction[A](id: UniqueIdentifier, body: Proof[A]) extends Proof[A]

  // given A -> B, A proves B
  final case class ModusPonens[A](implication: Proof[A], premise: Proof[A]) extends Proof[A]

  // given A -> B, not B proves not A
  final case class ModusTollens[A](implication: Proof[A], consequentFalse: Proof[A]) extends Proof[A]

  // given A, B proves A & B
  final case class ConjunctIntro[A](a: Proof[A], b: Proof[A]) extends Proof[A]

  // given A proves A | B
  final case class DisjunctIntroLeft[A](p: Proof[A]) extends Proof[A]

  // given B proves A | B
  final case class DisjunctIntroRight[A](p: Proof[A]) extends Proof[A]

  // given A | B proves B | A
  final case class DisjunctCommute[A](p: Proof[A]) extends Proof[A]

  // given A & B proves B & A
  final case class ConjunctCommute[A](p: Proof[A]) extends Proof[A]

  // given (not A) & (not B) proves not (A | B)
  final case class DeMorganAndPullNotOut[A](p: Proof[A]) extends Proof[A]

  // given (not A) | (not B) proves not (A & B)
  final case class DeMorganOrPullNotOut[A](p: Proof[A]) extends Proof[A]

  // given not (A & B) proves (not A) | (not B)
  final case class DeMorganAndPushNotIn[A](p: Proof[A]) extends Proof[A]

  // given not (A | B) proves (not A) & (not B)
  final case class DeMorganOrPushNotIn[A](p: Proof[A]) extends Proof[A]

  // given A proves not not A
  final case class DoubleNegIntro[A](p: Proof[A]) extends Proof[A]

  // given A, not A proves false
  final case class Contradiction[A](p: Proof[A], notP: Proof[A]) extends Proof[A]

  // given P -> Q, Q -> R proves P -> R
  final case class HypotheticalSyllogism[A](pImpliesQ: Proof[A], qImpliesR: Proof[A]) extends Proof[A]
}
