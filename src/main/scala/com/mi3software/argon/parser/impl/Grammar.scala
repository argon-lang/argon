package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser.GrammarError
import com.mi3software.argon.parser.impl.Grammar.{ParseOptions, ParseState}
import com.mi3software.argon.util._

import scala.collection.immutable._
import scala.language.postfixOps
import scalaz._
import Scalaz._

sealed trait Grammar[TToken, TSyntaxError, TLabel, +T] {

  type TErrorList = NonEmptyList[TSyntaxError]
  protected type TParseState = ParseState[TToken]
  protected type TParseOptions = ParseOptions[TToken, TSyntaxError, TLabel]


  final def parse(tokens: Vector[WithSource[TToken]], pos: FilePosition): Either[TErrorList, (Vector[WithSource[TToken]], FilePosition, WithSource[T])] =
    parseImpl(ParseState(tokens, pos), ParseOptions(Set.empty, None)).map {
      case (ParseState(tokens2, pos2), result) => (tokens2, pos2, result)
    }

  protected def parseImpl(state: TParseState, options: TParseOptions): Either[TErrorList, (TParseState, WithSource[T])]

}

object Grammar {

  private[impl] final case class ParseState[TToken](tokens: Vector[WithSource[TToken]], pos: FilePosition)
  private[impl] final case class ParseOptions[TToken, TSyntaxError, TLabel](leftRecRules: Set[Grammar[TToken, TSyntaxError, TLabel, _]], currentLabel: Option[TLabel]) {

    def notLeftRec: ParseOptions[TToken, TSyntaxError, TLabel] =
      if(leftRecRules.isEmpty)
        this
      else
        copy(leftRecRules = Set.empty)

    def addLeftRec(rule: Grammar[TToken, TSyntaxError, TLabel, _]): ParseOptions[TToken, TSyntaxError, TLabel] =
      copy(leftRecRules = leftRecRules + rule)

    def setLabel(label: TLabel): ParseOptions[TToken, TSyntaxError, TLabel] =
      copy(currentLabel = Some(label))

  }

  trait CombinerBase {

    trait GrammarConcatCombiner[A, B, T] {
      def combine(a: A, b: B): T
    }


    implicit def valueConcatCombiner[A, B]: GrammarConcatCombiner[A, B, (A, B)] = (a, b) => (a, b)
  }

  object Operators extends CombinerBase {

    final implicit class GrammarOperatorsImpl[TToken, TSyntaxError, TLabel, T](grammar1: => Grammar[TToken, TSyntaxError, TLabel, T]) {

      def --> [U](f: T => U): Grammar[TToken, TSyntaxError, TLabel, U] = -+>(WithSource.lift(f))
      def -+> [U](f: WithSource[T] => WithSource[U]): Grammar[TToken, TSyntaxError, TLabel, U] =
        new MapGrammar[TToken, TSyntaxError, TLabel, T, U](grammar1, f)

      def | [U >: T]
      (grammar2: => Grammar[TToken, TSyntaxError, TLabel, U])
      (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
      : Grammar[TToken, TSyntaxError, TLabel, U] =
        new UnionGrammar[TToken, TSyntaxError, TLabel, U](grammar1, grammar2)

      def ++ [U, V]
      (grammar2: => Grammar[TToken, TSyntaxError, TLabel, U])
      (implicit combiner: GrammarConcatCombiner[T, U, V], errorFactory: ErrorFactory[TToken, _, TSyntaxError])
      : Grammar[TToken, TSyntaxError, TLabel, V] =
        ConcatGrammar(grammar1, grammar2) { (a, b) => WithSource(combiner.combine(a.value, b.value), SourceLocation.merge(a.location, b.location)) }

      final class LeftRecGrammarBuilder[U]
      (grammar2: => Grammar[TToken, TSyntaxError, TLabel, U])
      (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError]) {

        def --> (f: (T, U) => T): Grammar[TToken, TSyntaxError, TLabel, T] =
          LeftRecGrammar(grammar1, grammar2) { (a, b) => WithSource(f(a.value, b.value), SourceLocation.merge(a.location, b.location)) }

        def -\> (f: (WithSource[T], U) => T): Grammar[TToken, TSyntaxError, TLabel, T] =
          LeftRecGrammar(grammar1, grammar2) { (a, b) => WithSource(f(a, b.value), SourceLocation.merge(a.location, b.location)) }

      }

      def -- [U]
      (grammar2: => Grammar[TToken, TSyntaxError, TLabel, U])
      (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
      : LeftRecGrammarBuilder[U] =
        new LeftRecGrammarBuilder[U](grammar2)

      def discard: Grammar[TToken, TSyntaxError, TLabel, Unit] = --> { _ => () }
      def ? (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError]): Grammar[TToken, TSyntaxError, TLabel, Option[T]] =
        --> (Some.apply) | EmptyStrGrammar(None)

      def +~
      (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
      : Grammar[TToken, TSyntaxError, TLabel, NonEmptyList[T]] =
        this ++ (this*) --> { case (head, tail) => NonEmptyList.nel(head, tail.toIList) }

      def *
      (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
      : Grammar[TToken, TSyntaxError, TLabel, Vector[T]] =
        new RepeatGrammar(grammar1)

      def observeSource: Grammar[TToken, TSyntaxError, TLabel, WithSource[T]] = -+> {
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

  import Operators._

  def label[TToken, TSyntaxError, TLabel, T](label: TLabel)(inner: => Grammar[TToken, TSyntaxError, TLabel, T]): Grammar[TToken, TSyntaxError, TLabel, T] =
    new LabelGrammar(label, inner)

  def token[TToken, TSyntaxError, TLabel, TTokenCategory]
  (category: TTokenCategory, tokenMatches: TToken => Boolean)
  (implicit errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError])
  : Grammar[TToken, TSyntaxError, TLabel, TToken] =
    matcher(category, t => Some(t).filter(tokenMatches))

  def tokenSource[TToken, TSyntaxError, TLabel, TTokenCategory]
  (category: TTokenCategory, tokenMatches: WithSource[TToken] => Boolean)
  (implicit errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError])
  : Grammar[TToken, TSyntaxError, TLabel, TToken] =
    matcherSource(category, t => Some(t).filter(tokenMatches))

  def matcher[TToken, TSyntaxError, TLabel, TTokenCategory, Result]
  (category: TTokenCategory, tokenMatcher: TToken => Option[Result])
  (implicit errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError])
  : Grammar[TToken, TSyntaxError, TLabel, Result] =
    matcherSource(category, WithSource.liftF(tokenMatcher))

  def matcherSource[TToken, TSyntaxError, TLabel, TTokenCategory, Result]
  (category: TTokenCategory, tokenMatcher: TokenMatcher[TToken, Result])
  (implicit errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError])
  : Grammar[TToken, TSyntaxError, TLabel, Result] =
    TokenGrammar(category, tokenMatcher)

  def partialMatcher[TToken, TSyntaxError, TLabel, TTokenCategory, Result]
  (category: TTokenCategory)
  (f: PartialFunction[TToken, Result])
  (implicit errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError])
  : Grammar[TToken, TSyntaxError, TLabel, Result] =
    matcher(category, f.lift)

  def parseAllImpl[TToken, TSyntaxError, TLabel, T, U](rule: Grammar[TToken, TSyntaxError, TLabel, T])(collector: WithSource[T] => Option[U])(tokens: Vector[WithSource[TToken]], pos: FilePosition, acc: Vector[U]): Either[NonEmptyList[TSyntaxError], Vector[U]] =
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

  def parseAll[TToken, TSyntaxError, TLabel, T, U](rule: Grammar[TToken, TSyntaxError, TLabel, T])(collector: WithSource[T] => Option[U])(tokens: Vector[WithSource[TToken]], pos: FilePosition): Either[NonEmptyList[TSyntaxError], Vector[U]] =
    parseAllImpl(rule)(collector)(tokens, pos, Vector.empty)

  trait ErrorFactory[-TToken, -TTokenCategory, TSyntaxError] {
    def createError(error: GrammarError[TToken, TTokenCategory]): TSyntaxError
    def createAmbiguityError(location: SourceLocation): TSyntaxError
    def errorEndLocationOrder: Order[TSyntaxError]
  }

  type TokenMatcher[TToken, T] = WithSource[TToken] => Option[WithSource[T]]

  private final case class RejectGrammar[TToken, TSyntaxError, TLabel, T](grammarErrors: NonEmptyList[TSyntaxError]) extends Grammar[TToken, TSyntaxError, TLabel, T] {

    override protected def parseImpl(state: TParseState, options: TParseOptions): Either[TErrorList, (TParseState, WithSource[T])] =
      Left(grammarErrors)

  }

  private final case class EmptyStrGrammar[TToken, TSyntaxError, TLabel, T]
  (result: T)
  (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
    extends Grammar[TToken, TSyntaxError, TLabel, T] {

    override protected def parseImpl(state: TParseState, options: TParseOptions): Either[TErrorList, (TParseState, WithSource[T])] =
      Right((state, WithSource(result, SourceLocation(state.pos, state.pos))))

  }

  private final case class TokenGrammar[TToken, TSyntaxError, TLabel, TTokenCategory, T]
  (
    category: TTokenCategory,
    tokenMatcher: TokenMatcher[TToken, T]
  )(implicit
    errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError]
  ) extends Grammar[TToken, TSyntaxError, TLabel, T] {

    object MatchingToken {
      def unapply(arg: WithSource[TToken]): Option[WithSource[T]] = tokenMatcher(arg)
    }


    override protected def parseImpl(state: TParseState, options: TParseOptions): Either[TErrorList, (TParseState, WithSource[T])] =
      state.tokens match {
        case (token @ MatchingToken(value)) +: tail =>
          Right((
            ParseState(
              tail,
              tail.headOption.map { _.location.start }.getOrElse { token.location.end }
            ),
            value
          ))
        case token +: _ => Left(NonEmptyList(errorFactory.createError(GrammarError.UnexpectedToken(category, token))))
        case Vector() => Left(NonEmptyList(errorFactory.createError(GrammarError.UnexpectedEndOfFile(category, state.pos))))
      }

  }

  private final class ConcatGrammar[TToken, TSyntaxError, TLabel, A, B, T]
  (
    grammarAUncached: => Grammar[TToken, TSyntaxError, TLabel, A],
    grammarBUncached: => Grammar[TToken, TSyntaxError, TLabel, B],
    combine: (WithSource[A], WithSource[B]) => WithSource[T]
  )(implicit
    errorFactory: ErrorFactory[TToken, _, TSyntaxError]
  ) extends Grammar[TToken, TSyntaxError, TLabel, T] {

    private lazy val grammarA = grammarAUncached
    private lazy val grammarB = grammarBUncached


    override protected def parseImpl(state: TParseState, options: TParseOptions): Either[TErrorList, (TParseState, WithSource[T])] =
      grammarA.parseImpl(state, options).flatMap {
        case (state2, valueA) =>
          grammarB.parseImpl(state2, options.notLeftRec).map {
            case (state3, valueB) =>
              (state3, combine(valueA, valueB))
          }
      }

  }

  private object ConcatGrammar {
    def apply[TToken, TSyntaxError, TLabel, A, B, T]
    (
      grammarA: => Grammar[TToken, TSyntaxError, TLabel, A],
      grammarB: => Grammar[TToken, TSyntaxError, TLabel, B]
    )(
      combine: (WithSource[A], WithSource[B]) => WithSource[T]
    )(implicit
      errorFactory: ErrorFactory[TToken, _, TSyntaxError]
    ): ConcatGrammar[TToken, TSyntaxError, TLabel, A, B, T] =
      new ConcatGrammar(grammarA, grammarB, combine)

  }

  private final class LeftRecGrammar[TToken, TSyntaxError, TLabel, A, B]
  (
    grammarAUncached: => Grammar[TToken, TSyntaxError, TLabel, A],
    grammarBUncached: => Grammar[TToken, TSyntaxError, TLabel, B],
    combine: (WithSource[A], WithSource[B]) => WithSource[A]
  )(implicit
    errorFactory: ErrorFactory[TToken, _, TSyntaxError]
  ) extends Grammar[TToken, TSyntaxError, TLabel, A] {

    private lazy val grammarA = grammarAUncached
    private val grammarBRep = grammarBUncached.observeSource*


    override protected def parseImpl(state: TParseState, options: TParseOptions): Either[TErrorList, (TParseState, WithSource[A])] =
      if(options.leftRecRules contains this)
        Left(NonEmptyList(errorFactory.createError(GrammarError.InfiniteRecursion(state.pos))))
      else
        grammarA.parseImpl(state, options.addLeftRec(this)).flatMap {
          case (state2, valueA) =>
            grammarBRep.parseImpl(state2, options.notLeftRec).map {
              case (state3, WithSource(valueBVec, _)) =>
                (state3, valueBVec.foldLeft(valueA)(combine))
            }
        }

  }

  private object LeftRecGrammar {
    def apply[TToken, TSyntaxError, TLabel, A, B]
    (
      grammarA: => Grammar[TToken, TSyntaxError, TLabel, A],
      grammarB: => Grammar[TToken, TSyntaxError, TLabel, B]
    )(
      combine: (WithSource[A], WithSource[B]) => WithSource[A]
    )(implicit
      errorFactory: ErrorFactory[TToken, _, TSyntaxError]
    ): LeftRecGrammar[TToken, TSyntaxError, TLabel, A, B] =
      new LeftRecGrammar(grammarA, grammarB, combine)

  }

  private final class UnionGrammar[TToken, TSyntaxError, TLabel, T]
  (grammarAUncached: => Grammar[TToken, TSyntaxError, TLabel, T], grammarBUncached: => Grammar[TToken, TSyntaxError, TLabel, T])
  (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
    extends Grammar[TToken, TSyntaxError, TLabel, T] {

    private lazy val grammarA = grammarAUncached
    private lazy val grammarB = grammarBUncached


    override protected def parseImpl(state: TParseState, options: TParseOptions): Either[TErrorList, (TParseState, WithSource[T])] =
      grammarA.parseImpl(state, options) match {
        case result @ Right(_) => result
        case Left(errorListA) =>
          grammarB.parseImpl(state, options) match {
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

    def apply[TToken, TSyntaxError, TLabel, T]
    (grammarA: => Grammar[TToken, TSyntaxError, TLabel, T], grammarB: => Grammar[TToken, TSyntaxError, TLabel, T])
    (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
    : Grammar[TToken, TSyntaxError, TLabel, T] =
      new UnionGrammar[TToken, TSyntaxError, TLabel, T](grammarA, grammarB)

    def fromList[TToken, TSyntaxError, TLabel, T]
    (grammars: NonEmptyList[Lazy[Grammar[TToken, TSyntaxError, TLabel, T]]])
    (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
    : Grammar[TToken, TSyntaxError, TLabel, T] =
      grammars.tail match {
        case ICons(head2, tail) =>
          tail.foldLeft(apply(grammars.head.value, head2.value)) { (a, b) => apply(a, b.value) }

        case INil() =>
          grammars.head.value
      }

  }

  private final class RepeatGrammar[TToken, TSyntaxError, TLabel, T]
  (innerUncached: => Grammar[TToken, TSyntaxError, TLabel, T])
  (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
    extends Grammar[TToken, TSyntaxError, TLabel, Vector[T]] {

    private lazy val inner = innerUncached


    override protected def parseImpl(state: TParseState, options: TParseOptions): Either[TErrorList, (TParseState, WithSource[Vector[T]])] =
      parseInner(state, options, Vector.empty)

    private def parseInner(state: TParseState, options: TParseOptions, items: Vector[WithSource[T]]): Either[TErrorList, (TParseState, WithSource[Vector[T]])] =
      inner.parseImpl(state, options) match {
        case Left(_) =>
          val location = items match {
            case WithSource(_, SourceLocation(start, _)) +: _ :+ WithSource(_, SourceLocation(_, end)) => SourceLocation(start, end)
            case Vector(WithSource(_, loc)) => loc
            case Vector() => SourceLocation(state.pos, state.pos)
          }

          Right((state, WithSource(items.map { case WithSource(item, _) => item }, location)))
        case Right((state2, item)) => parseInner(state2, options.notLeftRec, items :+ item)
      }

  }

  private final class MapGrammar[TToken, TSyntaxError, TLabel, T, U](innerUncached: => Grammar[TToken, TSyntaxError, TLabel, T], f: WithSource[T] => WithSource[U]) extends Grammar[TToken, TSyntaxError, TLabel, U] {

    private lazy val inner = innerUncached

    override protected def parseImpl(state: TParseState, options: TParseOptions): Either[TErrorList, (TParseState, WithSource[U])] =
      inner.parseImpl(state, options).map {
        case (state2, value) => (state2, f(value))
      }
  }

  private final class LabelGrammar[TToken, TSyntaxError, TLabel, T](label: TLabel, innerUncached: => Grammar[TToken, TSyntaxError, TLabel, T]) extends Grammar[TToken, TSyntaxError, TLabel, T] {

    private lazy val inner = innerUncached

    override protected def parseImpl(state: TParseState, options: TParseOptions): Either[TErrorList, (TParseState, WithSource[T])] =
      inner.parseImpl(state, options.setLabel(label))
    
  }

}
