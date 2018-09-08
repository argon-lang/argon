package com.mi3software.argon.grammar

import com.mi3software.argon.grammar.Grammar._
import com.mi3software.argon.util._

import scala.collection.immutable.{ Stream => _, _ }
import scala.language.postfixOps
import scalaz._
import Scalaz._
import fs2._

sealed trait Grammar[TToken, TSyntaxError, TLabel <: RuleLabel, +T] {

  type TErrorList = NonEmptyList[TSyntaxError]
  type TParseState = ParseState[TToken]
  type TParseOptions = ParseOptions[TToken, TSyntaxError, TLabel]

  def parseTokens(state: TParseState, options: TParseOptions): GrammarResult[TToken, TSyntaxError, TLabel, T]
  def parseEnd(pos: FilePosition, options: TParseOptions): GrammarResultComplete[TToken, TSyntaxError, TLabel, T]

}

object Grammar {

  trait RuleLabel {
    type RuleType
  }

  trait GrammarFactory[TToken, TSyntaxError, TLabel <: RuleLabel] {
    type TGrammar[+T] = Grammar[TToken, TSyntaxError, TLabel, T]
    def apply[T](label: TLabel { type RuleType = T }): TGrammar[T]

    def rule(label: TLabel): Grammar[TToken, TSyntaxError, TLabel, label.RuleType] =
      new LabelRefGrammar[TToken, TSyntaxError, TLabel, label.RuleType](label)
  }

  sealed trait GrammarResult[TToken, TSyntaxError, TLabel <: RuleLabel, +T] {
    def map[U](f: (ParseState[TToken], WithSource[T]) => (ParseState[TToken], WithSource[U])): GrammarResult[TToken, TSyntaxError, TLabel, U]
    def flatMap[U](f: (ParseState[TToken], WithSource[T]) => GrammarResult[TToken, TSyntaxError, TLabel, U]): GrammarResult[TToken, TSyntaxError, TLabel, U]

    def treatFailureAsError: GrammarResult[TToken, TSyntaxError, TLabel, T]

    def transformComplete[U](f: GrammarResultComplete[TToken, TSyntaxError, TLabel, T] => GrammarResult[TToken, TSyntaxError, TLabel, U]): GrammarResult[TToken, TSyntaxError, TLabel, U]
    def completeResult: GrammarResultComplete[TToken, TSyntaxError, TLabel, T]
  }

  sealed trait GrammarResultNonSuccess[TToken, TSyntaxError, TLabel <: RuleLabel, +T] extends GrammarResult[TToken, TSyntaxError, TLabel, T]

  sealed trait GrammarResultComplete[TToken, TSyntaxError, TLabel <: RuleLabel, +T] extends GrammarResult[TToken, TSyntaxError, TLabel, T] {
    def toEither: Either[NonEmptyList[TSyntaxError], (ParseState[TToken], WithSource[T])]

    def flatMap[U](f: (ParseState[TToken], WithSource[T]) => GrammarResultComplete[TToken, TSyntaxError, TLabel, U]): GrammarResultComplete[TToken, TSyntaxError, TLabel, U]


    override def map[U](f: (ParseState[TToken], WithSource[T]) => (ParseState[TToken], WithSource[U])): GrammarResultComplete[TToken, TSyntaxError, TLabel, U]
    override def flatMap[U](f: (ParseState[TToken], WithSource[T]) => GrammarResult[TToken, TSyntaxError, TLabel, U]): GrammarResult[TToken, TSyntaxError, TLabel, U]
    override def treatFailureAsError: GrammarResultComplete[TToken, TSyntaxError, TLabel, T]


    override def transformComplete[U](f: GrammarResultComplete[TToken, TSyntaxError, TLabel, T] => GrammarResult[TToken, TSyntaxError, TLabel, U]): GrammarResult[TToken, TSyntaxError, TLabel, U] =
      f(this)

    override def completeResult: GrammarResultComplete[TToken, TSyntaxError, TLabel, T] = this
  }

  final case class GrammarResultSuccess[TToken, TSyntaxError, TLabel <: RuleLabel, +T](parseState: ParseState[TToken], value: WithSource[T]) extends GrammarResultComplete[TToken, TSyntaxError, TLabel, T] {
    override def map[U](f: (ParseState[TToken], WithSource[T]) => (ParseState[TToken], WithSource[U])): GrammarResultComplete[TToken, TSyntaxError, TLabel, U] =
      f(parseState, value) match { case (parseState2, value2) => GrammarResultSuccess(parseState2, value2) }

    override def flatMap[U](f: (ParseState[TToken], WithSource[T]) => GrammarResult[TToken, TSyntaxError, TLabel, U]): GrammarResult[TToken, TSyntaxError, TLabel, U] =
      f(parseState, value)

    override def flatMap[U](f: (ParseState[TToken], WithSource[T]) => GrammarResultComplete[TToken, TSyntaxError, TLabel, U]): GrammarResultComplete[TToken, TSyntaxError, TLabel, U] =
      f(parseState, value)

    override def treatFailureAsError: GrammarResultComplete[TToken, TSyntaxError, TLabel, T] = this

    override def toEither: Either[NonEmptyList[TSyntaxError], (ParseState[TToken], WithSource[T])] = Right((parseState, value))
  }

  final case class GrammarResultFailure[TToken, TSyntaxError, TLabel <: RuleLabel](failure: NonEmptyList[TSyntaxError])
    extends GrammarResultComplete[TToken, TSyntaxError, TLabel, Nothing]
      with GrammarResultNonSuccess[TToken, TSyntaxError, TLabel, Nothing] {


    override def map[U](f: (ParseState[TToken], WithSource[Nothing]) => (ParseState[TToken], WithSource[U])): GrammarResultComplete[TToken, TSyntaxError, TLabel, U] = this

    override def flatMap[U](f: (ParseState[TToken], WithSource[Nothing]) => GrammarResult[TToken, TSyntaxError, TLabel, U]): GrammarResult[TToken, TSyntaxError, TLabel, U] = this
    override def flatMap[U](f: (ParseState[TToken], WithSource[Nothing]) => GrammarResultComplete[TToken, TSyntaxError, TLabel, U]): GrammarResultComplete[TToken, TSyntaxError, TLabel, U] = this

    override def treatFailureAsError: GrammarResultComplete[TToken, TSyntaxError, TLabel, Nothing] = GrammarResultError(failure)

    override def toEither: Either[NonEmptyList[TSyntaxError], Nothing] = Left(failure)
  }

  final case class GrammarResultError[TToken, TSyntaxError, TLabel <: RuleLabel](error: NonEmptyList[TSyntaxError])
    extends GrammarResultComplete[TToken, TSyntaxError, TLabel, Nothing]
      with GrammarResultNonSuccess[TToken, TSyntaxError, TLabel, Nothing] {


    override def map[U](f: (ParseState[TToken], WithSource[Nothing]) => (ParseState[TToken], WithSource[U])): GrammarResultComplete[TToken, TSyntaxError, TLabel, U] = this

    override def flatMap[U](f: (ParseState[TToken], WithSource[Nothing]) => GrammarResult[TToken, TSyntaxError, TLabel, U]): GrammarResult[TToken, TSyntaxError, TLabel, U] = this
    override def flatMap[U](f: (ParseState[TToken], WithSource[Nothing]) => GrammarResultComplete[TToken, TSyntaxError, TLabel, U]): GrammarResultComplete[TToken, TSyntaxError, TLabel, U] = this

    override def treatFailureAsError: GrammarResultComplete[TToken, TSyntaxError, TLabel, Nothing] = this

    override def toEither: Either[NonEmptyList[TSyntaxError], Nothing] = Left(error)
  }

  final class GrammarResultTransform[TToken, TSyntaxError, TLabel <: RuleLabel, T, +U]
  (
    val grammar: Grammar[TToken, TSyntaxError, TLabel, T],
    val parseOptions: ParseOptions[TToken, TSyntaxError, TLabel],
    val pos: FilePosition,
    val f: GrammarResultComplete[TToken, TSyntaxError, TLabel, T] => GrammarResult[TToken, TSyntaxError, TLabel, U]
  ) extends GrammarResult[TToken, TSyntaxError, TLabel, U]
    with GrammarResultNonSuccess[TToken, TSyntaxError, TLabel, U] {
    override def map[V](f2: (ParseState[TToken], WithSource[U]) => (ParseState[TToken], WithSource[V])): GrammarResult[TToken, TSyntaxError, TLabel, V] =
      transformComplete(_.map(f2))

    override def flatMap[V](f2: (ParseState[TToken], WithSource[U]) => GrammarResult[TToken, TSyntaxError, TLabel, V]): GrammarResult[TToken, TSyntaxError, TLabel, V] =
      transformComplete(_.flatMap(f2))

    override def treatFailureAsError: GrammarResult[TToken, TSyntaxError, TLabel, U] =
      transformComplete(_.treatFailureAsError)

    override def transformComplete[V](f2: GrammarResultComplete[TToken, TSyntaxError, TLabel, U] => GrammarResult[TToken, TSyntaxError, TLabel, V]): GrammarResult[TToken, TSyntaxError, TLabel, V] =
      GrammarResultTransform(grammar)(parseOptions)(pos)(tG => f(tG).transformComplete(f2))

    override def completeResult: GrammarResultComplete[TToken, TSyntaxError, TLabel, U] =
      f(grammar.parseEnd(pos, parseOptions)).completeResult
  }

  object GrammarResultTransform {
    def apply[TToken, TSyntaxError, TLabel <: RuleLabel, T, U]
    (prev: Grammar[TToken, TSyntaxError, TLabel, T])
    (parseOptions: ParseOptions[TToken, TSyntaxError, TLabel])
    (pos: FilePosition)
    (f: GrammarResultComplete[TToken, TSyntaxError, TLabel, T] => GrammarResult[TToken, TSyntaxError, TLabel, U])
    : GrammarResultTransform[TToken, TSyntaxError, TLabel, T, U] =
      new GrammarResultTransform(prev, parseOptions, pos, f)
  }

  final case class ParseState[TToken](tokens: Vector[WithSource[TToken]], pos: FilePosition)
  final case class ParseOptions[TToken, TSyntaxError, TLabel <: RuleLabel](leftRecRules: Set[Grammar[TToken, TSyntaxError, TLabel, _]], currentLabel: Option[TLabel], factory: GrammarFactory[TToken, TSyntaxError, TLabel]) {

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

    final implicit class GrammarOperatorsImpl[TToken, TSyntaxError, TLabel <: RuleLabel, T](grammar1: => Grammar[TToken, TSyntaxError, TLabel, T]) {

      def --> [U](f: T => U): Grammar[TToken, TSyntaxError, TLabel, U] = -+>(WithSource.lift(f))
      def -+> [U](f: WithSource[T] => WithSource[U]): Grammar[TToken, TSyntaxError, TLabel, U] =
        new MapGrammar[TToken, TSyntaxError, TLabel, T, U](grammar1, _.map { (state, value) => (state, f(value)) })

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

      def ++! [U, V]
      (grammar2: => Grammar[TToken, TSyntaxError, TLabel, U])
      (implicit combiner: GrammarConcatCombiner[T, U, V], errorFactory: ErrorFactory[TToken, _, TSyntaxError])
      : Grammar[TToken, TSyntaxError, TLabel, V] =
        ConcatGrammar(grammar1, StrictGrammar(grammar2)) { (a, b) => WithSource(combiner.combine(a.value, b.value), SourceLocation.merge(a.location, b.location)) }

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
      : Grammar[TToken, TSyntaxError, TLabel, NonEmptyList[T]] = {
        lazy val grammar1Cached = grammar1
        grammar1Cached ++ (grammar1Cached*) --> { case (head, tail) => NonEmptyList.nel(head, tail.toIList) }
      }

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

  def token[TToken, TSyntaxError, TLabel <: RuleLabel, TTokenCategory]
  (category: TTokenCategory, tokenMatches: TToken => Boolean)
  (implicit errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError])
  : Grammar[TToken, TSyntaxError, TLabel, TToken] =
    matcher(category, (t: TToken) => Some(t).filter(tokenMatches))

  def matcher[TToken, TSyntaxError, TLabel <: RuleLabel, TTokenCategory, Result]
  (category: TTokenCategory, tokenMatcher: TToken => Option[Result])
  (implicit errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError])
  : Grammar[TToken, TSyntaxError, TLabel, Result] =
    matcher(category, TokenMatcher.Anything(tokenMatcher))

  def partialMatcher[TToken, TSyntaxError, TLabel <: RuleLabel, TTokenCategory, Result]
  (category: TTokenCategory)
  (f: PartialFunction[TToken, Result])
  (implicit errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError])
  : Grammar[TToken, TSyntaxError, TLabel, Result] =
    matcher(category, f.lift)

  def matcher[TToken, TSyntaxError, TLabel <: RuleLabel, TTokenCategory, Result]
  (category: TTokenCategory, tokenMatcher: TokenMatcher[TToken, Result])
  (implicit errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError])
  : Grammar[TToken, TSyntaxError, TLabel, Result] =
    TokenGrammar(category, tokenMatcher)

  def parseAll[F[_]: Monad, TToken, TSyntaxError, TLabel <: RuleLabel, T](factory: GrammarFactory[TToken, TSyntaxError, TLabel])(label: TLabel { type RuleType = T })(pos: FilePosition): Pipe[EitherT[F, NonEmptyList[TSyntaxError], ?], WithSource[TToken], T] = {

    val defaultParseOptions: ParseOptions[TToken, TSyntaxError, TLabel] = ParseOptions(Set.empty, None, factory)
    val rule = factory(label)
    def defaultTransformRule(pos: FilePosition) = GrammarResultTransform(rule)(defaultParseOptions)(pos)(identity)

    def handleParseError(error: NonEmptyList[TSyntaxError]): Pull[EitherT[F, NonEmptyList[TSyntaxError], ?], T, Unit] =
      Pull.eval[EitherT[F, NonEmptyList[TSyntaxError], ?], Unit](EitherT(Monad[F].point(\/.left[NonEmptyList[TSyntaxError], Unit](error))))

    final case class TransformStepResult(items: Vector[T], parseResult: GrammarResultNonSuccess[TToken, TSyntaxError, TLabel, T], transPartial: Boolean)

    def parseAllPartial[A](trans: GrammarResultTransform[TToken, TSyntaxError, TLabel, A, T])(transPartial: Boolean)(tokens: Vector[WithSource[TToken]])(acc: Vector[T]): TransformStepResult =
      if(tokens.isEmpty)
        TransformStepResult(acc, trans, transPartial)
      else
        trans.grammar.parseTokens(ParseState(tokens, trans.pos), trans.parseOptions).transformComplete(trans.f) match {
          case GrammarResultSuccess(ParseState(tokens, pos), WithSource(value, _)) =>
            parseAllPartial(defaultTransformRule(pos))(transPartial = false)(tokens)(acc :+ value)

          case result: GrammarResultNonSuccess[TToken, TSyntaxError, TLabel, T] =>
            TransformStepResult(acc, result, transPartial = true)
        }

    def handleEndResult(result: GrammarResultComplete[TToken, TSyntaxError, TLabel, T]): Pull[EitherT[F, NonEmptyList[TSyntaxError], ?], T, Unit] =
      result match {
        case GrammarResultSuccess(state, WithSource(value, _)) =>
          Pull.output1(value).flatMap[EitherT[F, NonEmptyList[TSyntaxError], ?], T, Unit](_ => parseEndTokens(state))

        case GrammarResultFailure(failure) =>
          handleParseError(failure)

        case GrammarResultError(error) =>
          handleParseError(error)
      }

    def parseEndTokens(state: ParseState[TToken]): Pull[EitherT[F, NonEmptyList[TSyntaxError], ?], T, Unit] =
      if(state.tokens.isEmpty)
        Pull.done
      else
        handleEndResult(rule.parseTokens(state, defaultParseOptions).completeResult)

    def parseEnd[A](trans: GrammarResultTransform[TToken, TSyntaxError, TLabel, A, T]): Pull[EitherT[F, NonEmptyList[TSyntaxError], ?], T, Unit] =
      handleEndResult(trans.completeResult)

    def impl[A](trans: GrammarResultTransform[TToken, TSyntaxError, TLabel, A, T])(transPartial: Boolean)(s: fs2.Stream[EitherT[F, NonEmptyList[TSyntaxError], ?], WithSource[TToken]]): Pull[EitherT[F, NonEmptyList[TSyntaxError], ?], T, Unit] =
      Stream.InvariantOps[EitherT[F, NonEmptyList[TSyntaxError], ?], WithSource[TToken]](s).pull.unconsChunk.flatMap[EitherT[F, NonEmptyList[TSyntaxError], ?], T, Unit] {
        case Some((head, tail)) =>
          val TransformStepResult(items, parseResult, transPartialNext) = parseAllPartial(trans)(transPartial)(head.toVector)(Vector.empty)

          Pull.outputChunk(Chunk.vector(items)).flatMap[EitherT[F, NonEmptyList[TSyntaxError], ?], T, Unit] { _ =>
            parseResult match {
              case GrammarResultFailure(failure) =>
                handleParseError(failure)

              case GrammarResultError(error) =>
                handleParseError(error)

              case trans: GrammarResultTransform[TToken, TSyntaxError, TLabel, a, T] =>
                impl(trans)(transPartialNext)(tail)
            }
          }

        case None =>
          if(transPartial)
            parseEnd(trans)
          else
            Pull.done
      }

    s => impl(defaultTransformRule(FilePosition(1, 1)))(transPartial = false)(s).stream
  }

  trait ErrorFactory[-TToken, -TTokenCategory, TSyntaxError] {
    def createError(error: GrammarError[TToken, TTokenCategory]): TSyntaxError
    def createAmbiguityError(location: SourceLocation): TSyntaxError
    def errorEndLocationOrder: Order[TSyntaxError]
  }

  type TokenMatcherFunc[TToken, T] = WithSource[TToken] => Option[WithSource[T]]

  private final case class RejectGrammar[TToken, TSyntaxError, TLabel <: RuleLabel, T](grammarErrors: NonEmptyList[TSyntaxError]) extends Grammar[TToken, TSyntaxError, TLabel, T] {

    override def parseTokens(state: TParseState, options: TParseOptions): GrammarResult[TToken, TSyntaxError, TLabel, T] =
      GrammarResultFailure(grammarErrors)

    override def parseEnd(pos: FilePosition, options: TParseOptions): GrammarResultComplete[TToken, TSyntaxError, TLabel, T] =
      GrammarResultFailure(grammarErrors)

  }

  private final case class EmptyStrGrammar[TToken, TSyntaxError, TLabel <: RuleLabel, T](result: T) extends Grammar[TToken, TSyntaxError, TLabel, T] {

    override def parseTokens(state: TParseState, options: TParseOptions): GrammarResult[TToken, TSyntaxError, TLabel, T] =
      GrammarResultSuccess(state, WithSource(result, SourceLocation(state.pos, state.pos)))

    override def parseEnd(pos: FilePosition, options: TParseOptions): GrammarResultComplete[TToken, TSyntaxError, TLabel, T] =
      GrammarResultSuccess(ParseState(Vector.empty, pos), WithSource(result, SourceLocation(pos, pos)))

  }

  private final case class TokenGrammar[TToken, TSyntaxError, TLabel <: RuleLabel, TTokenCategory, T]
  (
    category: TTokenCategory,
    tokenMatcher: TokenMatcher[TToken, T]
  )(implicit
    errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError]
  ) extends Grammar[TToken, TSyntaxError, TLabel, T] {


    override def parseTokens(state: TParseState, options: TParseOptions): GrammarResult[TToken, TSyntaxError, TLabel, T] = {

      object MatchingToken {
        def unapply(arg: TToken): Option[T] = tokenMatcher.matchToken(arg)
      }

      state.tokens match {
        case WithSource(token @ MatchingToken(value), loc) +: tail =>
          GrammarResultSuccess(
            ParseState(
              tail,
              tail.headOption.map { _.location.start }.getOrElse { loc.end }
            ),
            WithSource(value, loc)
          )
        case token +: _ => GrammarResultFailure(NonEmptyList(errorFactory.createError(GrammarError.UnexpectedToken(category, token))))
        case Vector() => GrammarResultTransform(this)(options)(state.pos)(identity)
      }
    }

    override def parseEnd(pos: FilePosition, options: TParseOptions): GrammarResultComplete[TToken, TSyntaxError, TLabel, T] =
      GrammarResultFailure(NonEmptyList(errorFactory.createError(GrammarError.UnexpectedEndOfFile(category, pos))))

  }

  private final class ConcatGrammar[TToken, TSyntaxError, TLabel <: RuleLabel, A, B, T]
  (
    grammarAUncached: => Grammar[TToken, TSyntaxError, TLabel, A],
    grammarBUncached: => Grammar[TToken, TSyntaxError, TLabel, B],
    combine: (WithSource[A], WithSource[B]) => WithSource[T]
  ) extends Grammar[TToken, TSyntaxError, TLabel, T] {

    private lazy val grammarA = grammarAUncached
    private lazy val grammarB = grammarBUncached


    override def parseTokens(state: TParseState, options: TParseOptions): GrammarResult[TToken, TSyntaxError, TLabel, T] =
      grammarA.parseTokens(state, options).flatMap {
        case (state2, valueA) =>
          grammarB.parseTokens(state2, options.notLeftRec).map {
            case (state3, valueB) =>
              (state3, combine(valueA, valueB))
          }
      }

    override def parseEnd(pos: FilePosition, options: TParseOptions): GrammarResultComplete[TToken, TSyntaxError, TLabel, T] =
      grammarA.parseEnd(pos, options).flatMap {
        case (ParseState(Vector(), pos), valueA) =>
          grammarB.parseEnd(pos, options.notLeftRec).map {
            case (state3, valueB) =>
              (state3, combine(valueA, valueB))
          }

        case (state2, valueA) =>
          grammarB.parseTokens(state2, options.notLeftRec).completeResult.map {
            case (state3, valueB) =>
              (state3, combine(valueA, valueB))
          }
      }

  }

  private object ConcatGrammar {
    def apply[TToken, TSyntaxError, TLabel <: RuleLabel, A, B, T]
    (
      grammarA: => Grammar[TToken, TSyntaxError, TLabel, A],
      grammarB: => Grammar[TToken, TSyntaxError, TLabel, B]
    )(
      combine: (WithSource[A], WithSource[B]) => WithSource[T]
    ): ConcatGrammar[TToken, TSyntaxError, TLabel, A, B, T] =
      new ConcatGrammar(grammarA, grammarB, combine)

  }

  private final class StrictGrammar[TToken, TSyntaxError, TLabel <: RuleLabel, T]
  (
    innerUncached: => Grammar[TToken, TSyntaxError, TLabel, T]
  ) extends Grammar[TToken, TSyntaxError, TLabel, T] {

    private lazy val inner = innerUncached

    override def parseTokens(state: TParseState, options: TParseOptions): GrammarResult[TToken, TSyntaxError, TLabel, T] =
      inner.parseTokens(state, options).treatFailureAsError

    override def parseEnd(pos: FilePosition, options: TParseOptions): GrammarResultComplete[TToken, TSyntaxError, TLabel, T] =
      inner.parseEnd(pos, options).treatFailureAsError
  }

  private object StrictGrammar {
    def apply[TToken, TSyntaxError, TLabel <: RuleLabel, T]
    (
      inner: => Grammar[TToken, TSyntaxError, TLabel, T]
    ): StrictGrammar[TToken, TSyntaxError, TLabel, T] =
      new StrictGrammar(inner)

  }

  private final class LeftRecGrammar[TToken, TSyntaxError, TLabel <: RuleLabel, A, B]
  (
    grammarAUncached: => Grammar[TToken, TSyntaxError, TLabel, A],
    grammarBUncached: => Grammar[TToken, TSyntaxError, TLabel, B],
    combine: (WithSource[A], WithSource[B]) => WithSource[A]
  )(implicit
    errorFactory: ErrorFactory[TToken, _, TSyntaxError]
  ) extends Grammar[TToken, TSyntaxError, TLabel, A] {

    private lazy val grammarA = grammarAUncached
    private val grammarBRep = grammarBUncached.observeSource*


    override def parseTokens(state: TParseState, options: TParseOptions): GrammarResult[TToken, TSyntaxError, TLabel, A] =
      if(options.leftRecRules contains this)
        GrammarResultFailure(NonEmptyList(errorFactory.createError(GrammarError.InfiniteRecursion(state.pos))))
      else
        grammarA.parseTokens(state, options.addLeftRec(this)).flatMap {
          case (state2, valueA) =>
            grammarBRep.parseTokens(state2, options.notLeftRec).map {
              case (state3, WithSource(valueBVec, _)) =>
                (state3, valueBVec.foldLeft(valueA)(combine))
            }
        }

    override def parseEnd(pos: FilePosition, options: TParseOptions): GrammarResultComplete[TToken, TSyntaxError, TLabel, A] =
      if(options.leftRecRules contains this)
        GrammarResultFailure(NonEmptyList(errorFactory.createError(GrammarError.InfiniteRecursion(pos))))
      else
        grammarA.parseEnd(pos, options.addLeftRec(this)).flatMap {
          case (ParseState(Vector(), pos), valueA) =>
            grammarBRep.parseEnd(pos, options.notLeftRec).map {
              case (state3, WithSource(valueBVec, _)) =>
                (state3, valueBVec.foldLeft(valueA)(combine))
            }

          case (state2, valueA) =>
            grammarBRep.parseTokens(state2, options.notLeftRec).completeResult.map {
              case (state3, WithSource(valueBVec, _)) =>
                (state3, valueBVec.foldLeft(valueA)(combine))
            }
        }

  }

  private object LeftRecGrammar {
    def apply[TToken, TSyntaxError, TLabel <: RuleLabel, A, B]
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

  private final class ParseStateGrammar[TToken, TSyntaxError, TLabel <: RuleLabel, T]
  (
    innerUncached: => Grammar[TToken, TSyntaxError, TLabel, T],
    prevState: ParseState[TToken],
    prevOptions: ParseOptions[TToken, TSyntaxError, TLabel]
  ) extends Grammar[TToken, TSyntaxError, TLabel, T] {

    private lazy val inner = innerUncached

    override def parseTokens(state: TParseState, options: TParseOptions): GrammarResult[TToken, TSyntaxError, TLabel, T] =
      inner.parseTokens(prevState.copy(tokens = prevState.tokens ++ state.tokens), prevOptions)

    override def parseEnd(pos: FilePosition, options: TParseOptions): GrammarResultComplete[TToken, TSyntaxError, TLabel, T] =
      inner.parseTokens(prevState, prevOptions).completeResult
  }

  private final class UnionGrammar[TToken, TSyntaxError, TLabel <: RuleLabel, T]
  (grammarAUncached: => Grammar[TToken, TSyntaxError, TLabel, T], grammarBUncached: => Grammar[TToken, TSyntaxError, TLabel, T])
  (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
    extends Grammar[TToken, TSyntaxError, TLabel, T] {

    private lazy val grammarA = grammarAUncached
    private lazy val grammarB = grammarBUncached

    override def parseTokens(state: TParseState, options: TParseOptions): GrammarResult[TToken, TSyntaxError, TLabel, T] =
      grammarA.parseTokens(state, options) match {
        case result @ GrammarResultSuccess(_, _) => result
        case result @ GrammarResultError(_) => result
        case trans: GrammarResultTransform[TToken, TSyntaxError, TLabel, a, T] =>
          GrammarResultTransform(UnionGrammar(
            new MapGrammar[TToken, TSyntaxError, TLabel, a, T](
              trans.grammar,
              _.transformComplete(trans.f)
            ),
            new ParseStateGrammar(grammarB, state, options)
          ))(trans.parseOptions)(trans.pos)(identity)

        case GrammarResultFailure(errorListA) =>
          grammarB.parseTokens(state, options).transformComplete {
            case result @ GrammarResultSuccess(_, _) => result
            case result @ GrammarResultError(_) => result
            case GrammarResultFailure(errorListB) =>

              def findLastErrorPos(errorList: TErrorList): TSyntaxError =
                errorList.maximum1(errorFactory.errorEndLocationOrder)


              GrammarResultFailure(
                errorFactory.errorEndLocationOrder(findLastErrorPos(errorListA), findLastErrorPos(errorListB)) match {
                  case Ordering.GT => errorListA
                  case Ordering.LT => errorListB
                  case Ordering.EQ => errorListA.append(errorListB)
                }
              )
          }
      }

    override def parseEnd(pos: FilePosition, options: TParseOptions): GrammarResultComplete[TToken, TSyntaxError, TLabel, T] =
      grammarA.parseEnd(pos, options) match {
        case result @ GrammarResultSuccess(_, _) => result
        case result @ GrammarResultError(_) => result
        case GrammarResultFailure(errorListA) =>
          grammarB.parseEnd(pos, options) match {
            case result @ GrammarResultSuccess(_, _) => result
            case result @ GrammarResultError(_) => result
            case GrammarResultFailure(errorListB) =>

              def findLastErrorPos(errorList: TErrorList): TSyntaxError =
                errorList.maximum1(errorFactory.errorEndLocationOrder)


              GrammarResultFailure(
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

    def apply[TToken, TSyntaxError, TLabel <: RuleLabel, T]
    (grammarA: => Grammar[TToken, TSyntaxError, TLabel, T], grammarB: => Grammar[TToken, TSyntaxError, TLabel, T])
    (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
    : Grammar[TToken, TSyntaxError, TLabel, T] =
      new UnionGrammar[TToken, TSyntaxError, TLabel, T](grammarA, grammarB)

    def fromList[TToken, TSyntaxError, TLabel <: RuleLabel, T]
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

  private final class RepeatGrammar[TToken, TSyntaxError, TLabel <: RuleLabel, T]
  (innerUncached: => Grammar[TToken, TSyntaxError, TLabel, T])
  (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
    extends Grammar[TToken, TSyntaxError, TLabel, Vector[T]] {

    private lazy val inner = innerUncached


    override def parseTokens(state: TParseState, options: TParseOptions): GrammarResult[TToken, TSyntaxError, TLabel, Vector[T]] =
      parseInner(state, options, Vector.empty)

    private def itemsLocation(state: TParseState, items: Vector[WithSource[T]]): SourceLocation =
      items match {
        case WithSource(_, SourceLocation(start, _)) +: _ :+ WithSource(_, SourceLocation(_, end)) => SourceLocation(start, end)
        case Vector(WithSource(_, loc)) => loc
        case Vector() => SourceLocation(state.pos, state.pos)
      }

    private def parseInner(state: TParseState, options: TParseOptions, items: Vector[WithSource[T]]): GrammarResult[TToken, TSyntaxError, TLabel, Vector[T]] =
      if(state.tokens.isEmpty)
        GrammarResultTransform(this)(options)(state.pos)(_.map {
          case (state2, WithSource(nextItems, SourceLocation(_, endPos))) =>
            val startPos = items.headOption.map(_.location.start).getOrElse(state.pos)

            (state2, WithSource(items.map(removeSource) ++ nextItems, SourceLocation(startPos, endPos)))
        })
      else
        inner.parseTokens(state, options) match {
          case result @ GrammarResultError(_) => result
          case GrammarResultFailure(_) =>
            GrammarResultSuccess(state, WithSource(items.map(removeSource), itemsLocation(state, items)))

          case GrammarResultSuccess(state2, item) => parseInner(state2, options.notLeftRec, items :+ item)

          case trans: GrammarResultTransform[TToken, TSyntaxError, TLabel, a, T] =>
            GrammarResultTransform(
              UnionGrammar(
                new MapGrammar[TToken, TSyntaxError, TLabel, a, Vector[T]](trans.grammar, _.transformComplete(trans.f).flatMap {
                  (state2, item) => parseInner(state2, options, items :+ item)
                }),
                new ParseStateGrammar(EmptyStrGrammar[TToken, TSyntaxError, TLabel, Vector[T]](items.map(removeSource)), state, options)
              )
            )(trans.parseOptions)(trans.pos)(identity)

        }

    override def parseEnd(pos: FilePosition, options: TParseOptions): GrammarResultComplete[TToken, TSyntaxError, TLabel, Vector[T]] =
      GrammarResultSuccess(ParseState(Vector.empty, pos), WithSource(Vector.empty, SourceLocation(pos, pos)))

    private def removeSource[A](ws: WithSource[A]): A = ws.value
  }


  private final class MapGrammar[TToken, TSyntaxError, TLabel <: RuleLabel, T, U](innerUncached: => Grammar[TToken, TSyntaxError, TLabel, T], f: GrammarResult[TToken, TSyntaxError, TLabel, T] => GrammarResult[TToken, TSyntaxError, TLabel, U]) extends Grammar[TToken, TSyntaxError, TLabel, U] {

    private lazy val inner = innerUncached


    override def parseTokens(state: TParseState, options: TParseOptions): GrammarResult[TToken, TSyntaxError, TLabel, U] =
      f(inner.parseTokens(state, options))

    override def parseEnd(pos: FilePosition, options: TParseOptions): GrammarResultComplete[TToken, TSyntaxError, TLabel, U] =
      f(inner.parseEnd(pos, options)).completeResult

  }

  private final class LabelRefGrammar[TToken, TSyntaxError, TLabel <: RuleLabel, T](label: TLabel { type RuleType = T }) extends Grammar[TToken, TSyntaxError, TLabel, T] {
    override def parseTokens(state: TParseState, options: TParseOptions): GrammarResult[TToken, TSyntaxError, TLabel, T] =
      options.factory(label).parseTokens(state, options)

    override def parseEnd(pos: FilePosition, options: TParseOptions): GrammarResultComplete[TToken, TSyntaxError, TLabel, T] =
      options.factory(label).parseEnd(pos, options)
  }

}
