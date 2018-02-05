package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser.GrammarError
import com.mi3software.argon.util._

import scala.collection.immutable._
import scala.language.postfixOps
import scalaz._
import Scalaz._

sealed trait Grammar[TToken, TSyntaxError, +T] {

  type TErrorList = NonEmptyList[TSyntaxError]
  type LeftRecRules = Set[Grammar[TToken, TSyntaxError, _]]

  final def parse(tokens: Vector[WithSource[TToken]], pos: FilePosition): Either[TErrorList, (Vector[WithSource[TToken]], FilePosition, WithSource[T])] =
    parseImpl(tokens, pos, Set.empty)

  protected def parseImpl(tokens: Vector[WithSource[TToken]], pos: FilePosition, leftRecRules: LeftRecRules): Either[TErrorList, (Vector[WithSource[TToken]], FilePosition, WithSource[T])]

}

object Grammar {

  trait CombinerBase {

    trait GrammarConcatCombiner[A, B, T] {
      def combine(a: A, b: B): T
    }


    implicit def valueConcatCombiner[A, B]: GrammarConcatCombiner[A, B, (A, B)] = (a, b) => (a, b)
  }

  object Operators extends CombinerBase {

    final implicit class GrammarOperatorsImpl[TToken, TSyntaxError, T](grammar1: => Grammar[TToken, TSyntaxError, T]) {

      def --> [U](f: T => U): Grammar[TToken, TSyntaxError, U] = -+>(WithSource.lift(f))
      def -+> [U](f: WithSource[T] => WithSource[U]): Grammar[TToken, TSyntaxError, U] =
        new MapGrammar[TToken, TSyntaxError, T, U](grammar1, f)

      def | [U >: T]
      (grammar2: => Grammar[TToken, TSyntaxError, U])
      (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
      : Grammar[TToken, TSyntaxError, U] =
        new UnionGrammar[TToken, TSyntaxError, U](grammar1, grammar2)

      def ++ [U, V]
      (grammar2: => Grammar[TToken, TSyntaxError, U])
      (implicit combiner: GrammarConcatCombiner[T, U, V], errorFactory: ErrorFactory[TToken, _, TSyntaxError])
      : Grammar[TToken, TSyntaxError, V] =
        ConcatGrammar(grammar1, grammar2) { (a, b) => WithSource(combiner.combine(a.value, b.value), SourceLocation.merge(a.location, b.location)) }

      def discard: Grammar[TToken, TSyntaxError, Unit] = --> { _ => () }
      def ? (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError]): Grammar[TToken, TSyntaxError, Option[T]] =
        --> (Some.apply) | EmptyStrGrammar(WithSource(None, SourceLocation.empty))

      def +~
      (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
      : Grammar[TToken, TSyntaxError, NonEmptyList[T]] =
        this ++ (this*) --> { case (head, tail) => NonEmptyList.nel(head, tail.toIList) }

      def *
      (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
      : Grammar[TToken, TSyntaxError, Vector[T]] =
        new RepeatGrammar(grammar1)

      def observeSource: Grammar[TToken, TSyntaxError, WithSource[T]] = -+> {
        case value @ WithSource(_, location) => WithSource(value, location)
      }

    }

    implicit def tuple2ConcatCombiner[A, B, C]: GrammarConcatCombiner[(A, B), C, (A, B, C)] =
      (t, c) => (t._1, t._2, c)

    implicit def tuple3ConcatCombiner[A, B, C, D]: GrammarConcatCombiner[(A, B, C), D, (A, B, C, D)] =
      (t, d) => (t._1, t._2, t._3, d)

    implicit def tuple4ConcatCombiner[A, B, C, D, E]: GrammarConcatCombiner[(A, B, C, D), E, (A, B, C, D, E)] =
      (t, e) => (t._1, t._2, t._3, t._4, e)

    implicit def tuple5ConcatCombiner[A, B, C, D, E, F]: GrammarConcatCombiner[(A, B, C, D, E), F, (A, B, C, D, E, F)] =
      (t, f) => (t._1, t._2, t._3, t._4, t._5, f)

    implicit def tuple6ConcatCombiner[A, B, C, D, E, F, G]: GrammarConcatCombiner[(A, B, C, D, E, F), G, (A, B, C, D, E, F, G)] =
      (t, g) => (t._1, t._2, t._3, t._4, t._5, t._6, g)

    implicit def tuple7ConcatCombiner[A, B, C, D, E, F, G, H]: GrammarConcatCombiner[(A, B, C, D, E, F, G), H, (A, B, C, D, E, F, G, H)] =
      (t, h) => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, h)

    implicit def tuple8ConcatCombiner[A, B, C, D, E, F, G, H, I]: GrammarConcatCombiner[(A, B, C, D, E, F, G, H), I, (A, B, C, D, E, F, G, H, I)] =
      (t, i) => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, i)

    implicit def tuple9ConcatCombiner[A, B, C, D, E, F, G, H, I, J]: GrammarConcatCombiner[(A, B, C, D, E, F, G, H, I), J, (A, B, C, D, E, F, G, H, I, J)] =
      (t, j) => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, j)

    implicit def tuple10ConcatCombiner[A, B, C, D, E, F, G, H, I, J, K]: GrammarConcatCombiner[(A, B, C, D, E, F, G, H, I, J), K, (A, B, C, D, E, F, G, H, I, J, K)] =
      (t, k) => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, k)

    implicit def tuple11ConcatCombiner[A, B, C, D, E, F, G, H, I, J, K, L]: GrammarConcatCombiner[(A, B, C, D, E, F, G, H, I, J, K), L, (A, B, C, D, E, F, G, H, I, J, K, L)] =
      (t, l) => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, l)

    implicit def tuple12ConcatCombiner[A, B, C, D, E, F, G, H, I, J, K, L, M]: GrammarConcatCombiner[(A, B, C, D, E, F, G, H, I, J, K, L), M, (A, B, C, D, E, F, G, H, I, J, K, L, M)] =
      (t, m) => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, m)

    implicit def tuple13ConcatCombiner[A, B, C, D, E, F, G, H, I, J, K, L, M, N]: GrammarConcatCombiner[(A, B, C, D, E, F, G, H, I, J, K, L, M), N, (A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
      (t, n) => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, n)

  }


  def token[TToken, TSyntaxError, TTokenCategory]
  (category: TTokenCategory, tokenMatches: TToken => Boolean)
  (implicit errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError])
  : Grammar[TToken, TSyntaxError, TToken] =
    matcher(category, t => Some(t).filter(tokenMatches))

  def tokenSource[TToken, TSyntaxError, TTokenCategory]
  (category: TTokenCategory, tokenMatches: WithSource[TToken] => Boolean)
  (implicit errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError])
  : Grammar[TToken, TSyntaxError, TToken] =
    matcherSource(category, t => Some(t).filter(tokenMatches))

  def matcher[TToken, TSyntaxError, TTokenCategory, Result]
  (category: TTokenCategory, tokenMatcher: TToken => Option[Result])
  (implicit errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError])
  : Grammar[TToken, TSyntaxError, Result] =
    matcherSource(category, WithSource.liftF(tokenMatcher))

  def matcherSource[TToken, TSyntaxError, TTokenCategory, Result]
  (category: TTokenCategory, tokenMatcher: TokenMatcher[TToken, Result])
  (implicit errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError])
  : Grammar[TToken, TSyntaxError, Result] =
    TokenGrammar(category, tokenMatcher)

  def partialMatcher[TToken, TSyntaxError, TTokenCategory, Result]
  (category: TTokenCategory)
  (f: PartialFunction[TToken, Result])
  (implicit errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError])
  : Grammar[TToken, TSyntaxError, Result] =
    matcher(category, f.lift)

  def parseAllImpl[TToken, TSyntaxError, T, U](rule: Grammar[TToken, TSyntaxError, T])(collector: WithSource[T] => Option[U])(tokens: Vector[WithSource[TToken]], pos: FilePosition, acc: Vector[U]): Either[NonEmptyList[TSyntaxError], Vector[U]] =
    if(tokens.isEmpty)
      Right(acc)
    else
      rule.parse(tokens, pos) match {
        case Left(errorList) => Left(errorList)
        case Right((tokens2, pos2, item)) =>
          collector(item) match {
            case Some(res) => parseAllImpl(rule)(collector)(tokens2, pos2, acc :+ res)
            case None => parseAllImpl(rule)(collector)(tokens2, pos2, acc)
          }
      }

  def parseAll[TToken, TSyntaxError, T, U](rule: Grammar[TToken, TSyntaxError, T])(collector: WithSource[T] => Option[U])(tokens: Vector[WithSource[TToken]], pos: FilePosition): Either[NonEmptyList[TSyntaxError], Vector[U]] =
    parseAllImpl(rule)(collector)(tokens, pos, Vector.empty)

  trait ErrorFactory[-TToken, -TTokenCategory, TSyntaxError] {
    def createError(error: GrammarError[TToken, TTokenCategory]): TSyntaxError
    def createAmbiguityError(location: SourceLocation): TSyntaxError
    def errorEndLocationOrder: Order[TSyntaxError]
  }

  type TokenMatcher[TToken, T] = WithSource[TToken] => Option[WithSource[T]]

  private final case class RejectGrammar[TToken, TSyntaxError, T](grammarErrors: NonEmptyList[TSyntaxError]) extends Grammar[TToken, TSyntaxError, T] {


    override protected def parseImpl(tokens: Vector[WithSource[TToken]], pos: FilePosition, leftRecRules: LeftRecRules): Either[TErrorList, (Vector[WithSource[TToken]], FilePosition, WithSource[T])] =
      Left(grammarErrors)

  }

  private final case class EmptyStrGrammar[TToken, TSyntaxError, T]
  (result: WithSource[T])
  (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
    extends Grammar[TToken, TSyntaxError, T] {


    override protected def parseImpl(tokens: Vector[WithSource[TToken]], pos: FilePosition, leftRecRules: LeftRecRules): Either[TErrorList, (Vector[WithSource[TToken]], FilePosition, WithSource[T])] =
      Right((tokens, pos, result))

  }

  private final case class TokenGrammar[TToken, TSyntaxError, TTokenCategory, T]
  (
    category: TTokenCategory,
    tokenMatcher: TokenMatcher[TToken, T]
  )(implicit
    errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError]
  ) extends Grammar[TToken, TSyntaxError, T] {

    object MatchingToken {
      def unapply(arg: WithSource[TToken]): Option[WithSource[T]] = tokenMatcher(arg)
    }


    override protected def parseImpl(tokens: Vector[WithSource[TToken]], pos: FilePosition, leftRecRules: LeftRecRules): Either[TErrorList, (Vector[WithSource[TToken]], FilePosition, WithSource[T])] =
      tokens match {
        case (token @ MatchingToken(value)) +: tail =>
          Right((
            tail,
            tail.headOption.map { _.location.start }.getOrElse { token.location.end },
            value
          ))
        case token +: _ => Left(NonEmptyList(errorFactory.createError(GrammarError.UnexpectedToken(category, token))))
        case Vector() => Left(NonEmptyList(errorFactory.createError(GrammarError.UnexpectedEndOfFile(category, pos))))
      }

  }

  private final class ConcatGrammar[TToken, TSyntaxError, A, B, T]
  (
    grammarAUncached: => Grammar[TToken, TSyntaxError, A],
    grammarBUncached: => Grammar[TToken, TSyntaxError, B],
    combine: (WithSource[A], WithSource[B]) => WithSource[T]
  )(implicit
    errorFactory: ErrorFactory[TToken, _, TSyntaxError]
  ) extends Grammar[TToken, TSyntaxError, T] {

    private lazy val grammarA = grammarAUncached
    private lazy val grammarB = grammarBUncached


    override protected def parseImpl(tokens: Vector[WithSource[TToken]], pos: FilePosition, leftRecRules: LeftRecRules): Either[TErrorList, (Vector[WithSource[TToken]], FilePosition, WithSource[T])] =
      if(leftRecRules contains this)
        Left(NonEmptyList(errorFactory.createError(GrammarError.InfiniteRecursion(pos))))
      else
        grammarA.parseImpl(tokens, pos, leftRecRules + this).flatMap {
          case (tokens2, pos2, valueA) =>
            grammarB.parse(tokens2, pos2).map {
              case (tokens3, pos3, valueB) =>
                (tokens3, pos3, combine(valueA, valueB))
            }
        }

  }

  private object ConcatGrammar {
    def apply[TToken, TSyntaxError, A, B, T]
    (
      grammarA: => Grammar[TToken, TSyntaxError, A],
      grammarB: => Grammar[TToken, TSyntaxError, B]
    )(
      combine: (WithSource[A], WithSource[B]) => WithSource[T]
    )(implicit
      errorFactory: ErrorFactory[TToken, _, TSyntaxError]
    ): ConcatGrammar[TToken, TSyntaxError, A, B, T] =
      new ConcatGrammar(grammarA, grammarB, combine)

  }

  private final class UnionGrammar[TToken, TSyntaxError, T]
  (grammarAUncached: => Grammar[TToken, TSyntaxError, T], grammarBUncached: => Grammar[TToken, TSyntaxError, T])
  (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
    extends Grammar[TToken, TSyntaxError, T] {

    private lazy val grammarA = grammarAUncached
    private lazy val grammarB = grammarBUncached


    override protected def parseImpl(tokens: Vector[WithSource[TToken]], pos: FilePosition, leftRecRules: LeftRecRules): Either[TErrorList, (Vector[WithSource[TToken]], FilePosition, WithSource[T])] =
      grammarA.parseImpl(tokens, pos, leftRecRules) match {
        case result @ Right(_) => result
        case Left(errorListA) =>
          grammarB.parseImpl(tokens, pos, leftRecRules) match {
            case result @ Right(_) => result
            case Left(errorListB) =>

              def findLastErrorPos(errorList: TErrorList): TSyntaxError =
                errorList.maximum1(errorFactory.errorEndLocationOrder)

              Left(
                errorFactory.errorEndLocationOrder(findLastErrorPos(errorListA), findLastErrorPos(errorListB)) match {
                  case Ordering.GT => errorListA
                  case Ordering.LT => errorListB
                  case Ordering.EQ => errorListA.append(errorListB)
                }
              )
          }
      }

  }

  object UnionGrammar {

    def apply[TToken, TSyntaxError, T]
    (grammarA: => Grammar[TToken, TSyntaxError, T], grammarB: => Grammar[TToken, TSyntaxError, T])
    (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
    : Grammar[TToken, TSyntaxError, T] =
      new UnionGrammar[TToken, TSyntaxError, T](grammarA, grammarB)

    def fromList[TToken, TSyntaxError, T]
    (grammars: NonEmptyList[Lazy[Grammar[TToken, TSyntaxError, T]]])
    (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
    : Grammar[TToken, TSyntaxError, T] =
      grammars.tail match {
        case ICons(head2, tail) =>
          tail.foldLeft(apply(grammars.head.value, head2.value)) { (a, b) => apply(a, b.value) }

        case INil() =>
          grammars.head.value
      }

  }

  private final class RepeatGrammar[TToken, TSyntaxError, T]
  (innerUncached: => Grammar[TToken, TSyntaxError, T])
  (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
    extends Grammar[TToken, TSyntaxError, Vector[T]] {

    private lazy val inner = innerUncached

    override protected def parseImpl(tokens: Vector[WithSource[TToken]], pos: FilePosition, leftRecRules: LeftRecRules): Either[TErrorList, (Vector[WithSource[TToken]], FilePosition, WithSource[Vector[T]])] =
      parseInner(tokens, pos, leftRecRules, Vector.empty)

    private def parseInner(tokens: Vector[WithSource[TToken]], pos: FilePosition, leftRecRules: LeftRecRules, items: Vector[WithSource[T]]): Either[TErrorList, (Vector[WithSource[TToken]], FilePosition, WithSource[Vector[T]])] =
      inner.parseImpl(tokens, pos, leftRecRules) match {
        case Left(_) =>
          val location = items match {
            case WithSource(_, SourceLocation(start, _)) +: _ :+ WithSource(_, SourceLocation(_, end)) => SourceLocation(start, end)
            case Vector(WithSource(_, loc)) => loc
            case Vector() => SourceLocation(pos, pos)
          }

          Right((tokens, pos, WithSource(items.map { case WithSource(item, _) => item }, location)))
        case Right((tokens2, pos2, item)) => parseInner(tokens2, pos2, Set.empty, items :+ item)
      }

  }

  private final class MapGrammar[TToken, TSyntaxError, T, U](innerUncached: => Grammar[TToken, TSyntaxError, T], f: WithSource[T] => WithSource[U]) extends Grammar[TToken, TSyntaxError, U] {

    private lazy val inner = innerUncached

    override protected def parseImpl(tokens: Vector[WithSource[TToken]], pos: FilePosition, leftRecRules: LeftRecRules): Either[TErrorList, (Vector[WithSource[TToken]], FilePosition, WithSource[U])] =
      inner.parseImpl(tokens, pos, leftRecRules).map {
        case (tokens2, pos2, value) => (tokens2, pos2, f(value))
      }
  }


}
