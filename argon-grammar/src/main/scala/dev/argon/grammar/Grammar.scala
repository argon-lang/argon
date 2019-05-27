package dev.argon.grammar

import dev.argon.grammar.Grammar._
import dev.argon.util._

import scala.collection.immutable.{Stream => _, _}
import scala.language.postfixOps
import cats._
import cats.data._
import cats.implicits._
import dev.argon.util.stream._

sealed trait Grammar[TToken, TSyntaxError, TLabel <: RuleLabel, +T] {

  type TErrorList = NonEmptyVector[TSyntaxError]
  type TParseOptions = ParseOptions[TToken, TSyntaxError, TLabel]

  def parseTokens(tokens: NonEmptyVector[WithSource[TToken]], options: TParseOptions): GrammarResult[TToken, TSyntaxError, TLabel, T]
  def parseEnd(pos: FilePosition, options: TParseOptions): GrammarResultComplete[TToken, TSyntaxError, TLabel, T]

}

object Grammar {

  trait RuleLabel {
    type RuleType
  }

  abstract class GrammarFactory[TToken, TSyntaxError, TLabel <: RuleLabel] {
    type TGrammar[+T] = Grammar[TToken, TSyntaxError, TLabel, T]

    @SuppressWarnings(Array("org.wartremover.warts.Var"))
    private var cache: Map[TLabel, AnyRef] = Map.empty

    protected def createGrammar[T](label: TLabel { type RuleType = T }): TGrammar[T]

    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
    final def apply[T](label: TLabel { type RuleType = T }): TGrammar[T] = {
      cache.get(label) match {
        case Some(value) => value.asInstanceOf[TGrammar[T]]
        case None =>
          val result = createGrammar(label)
          cache = cache.updated(label, result)
          result
      }
    }

    def rule(label: TLabel): Grammar[TToken, TSyntaxError, TLabel, label.RuleType] =
      new LabelRefGrammar[TToken, TSyntaxError, TLabel, label.RuleType](label)
  }

  sealed trait GrammarResult[TToken, TSyntaxError, TLabel <: RuleLabel, +T] {
    def map[U](f: (Vector[WithSource[TToken]], WithSource[T]) => (Vector[WithSource[TToken]], WithSource[U])): GrammarResult[TToken, TSyntaxError, TLabel, U]
    def flatMap[U](f: (Vector[WithSource[TToken]], WithSource[T]) => GrammarResult[TToken, TSyntaxError, TLabel, U]): GrammarResult[TToken, TSyntaxError, TLabel, U]

    def treatFailureAsError: GrammarResult[TToken, TSyntaxError, TLabel, T]

    def recoverFailure[U >: T](f: (Vector[WithSource[TToken]], GrammarResultFailure[TToken, TSyntaxError, TLabel]) => GrammarResult[TToken, TSyntaxError, TLabel, U]): GrammarResult[TToken, TSyntaxError, TLabel, U]

    def transformComplete[U](f: GrammarResultComplete[TToken, TSyntaxError, TLabel, T] => GrammarResult[TToken, TSyntaxError, TLabel, U]): GrammarResult[TToken, TSyntaxError, TLabel, U]
    def completeResult(pos: FilePosition): GrammarResultComplete[TToken, TSyntaxError, TLabel, T]
  }

  sealed trait GrammarResultNonSuccess[TToken, TSyntaxError, TLabel <: RuleLabel, +T] extends GrammarResult[TToken, TSyntaxError, TLabel, T]

  sealed trait GrammarResultComplete[TToken, TSyntaxError, TLabel <: RuleLabel, +T] extends GrammarResult[TToken, TSyntaxError, TLabel, T] {
    def toEither: Either[NonEmptyVector[TSyntaxError], (Vector[WithSource[TToken]], WithSource[T])]

    def flatMap[U](f: (Vector[WithSource[TToken]], WithSource[T]) => GrammarResultComplete[TToken, TSyntaxError, TLabel, U]): GrammarResultComplete[TToken, TSyntaxError, TLabel, U]


    override def map[U](f: (Vector[WithSource[TToken]], WithSource[T]) => (Vector[WithSource[TToken]], WithSource[U])): GrammarResultComplete[TToken, TSyntaxError, TLabel, U]
    override def flatMap[U](f: (Vector[WithSource[TToken]], WithSource[T]) => GrammarResult[TToken, TSyntaxError, TLabel, U]): GrammarResult[TToken, TSyntaxError, TLabel, U]
    override def treatFailureAsError: GrammarResultComplete[TToken, TSyntaxError, TLabel, T]

    override def transformComplete[U](f: GrammarResultComplete[TToken, TSyntaxError, TLabel, T] => GrammarResult[TToken, TSyntaxError, TLabel, U]): GrammarResult[TToken, TSyntaxError, TLabel, U] =
      f(this)

    override def completeResult(pos: FilePosition): GrammarResultComplete[TToken, TSyntaxError, TLabel, T] = this
  }

  final case class GrammarResultSuccess[TToken, TSyntaxError, TLabel <: RuleLabel, +T](tokens: Vector[WithSource[TToken]], value: WithSource[T]) extends GrammarResultComplete[TToken, TSyntaxError, TLabel, T] {
    override def map[U](f: (Vector[WithSource[TToken]], WithSource[T]) => (Vector[WithSource[TToken]], WithSource[U])): GrammarResultComplete[TToken, TSyntaxError, TLabel, U] =
      f(tokens, value) match { case (tokens2, value2) => GrammarResultSuccess(tokens2, value2) }

    override def flatMap[U](f: (Vector[WithSource[TToken]], WithSource[T]) => GrammarResult[TToken, TSyntaxError, TLabel, U]): GrammarResult[TToken, TSyntaxError, TLabel, U] =
      f(tokens, value)

    override def flatMap[U](f: (Vector[WithSource[TToken]], WithSource[T]) => GrammarResultComplete[TToken, TSyntaxError, TLabel, U]): GrammarResultComplete[TToken, TSyntaxError, TLabel, U] =
      f(tokens, value)

    override def treatFailureAsError: GrammarResultComplete[TToken, TSyntaxError, TLabel, T] = this

    override def recoverFailure[U >: T](f: (Vector[WithSource[TToken]], GrammarResultFailure[TToken, TSyntaxError, TLabel]) => GrammarResult[TToken, TSyntaxError, TLabel, U]): GrammarResult[TToken, TSyntaxError, TLabel, U] =
      this

    override def toEither: Either[NonEmptyVector[TSyntaxError], (Vector[WithSource[TToken]], WithSource[T])] = Right((tokens, value))
  }

  final case class GrammarResultFailure[TToken, TSyntaxError, TLabel <: RuleLabel](failure: NonEmptyVector[TSyntaxError])
    extends GrammarResultComplete[TToken, TSyntaxError, TLabel, Nothing]
      with GrammarResultNonSuccess[TToken, TSyntaxError, TLabel, Nothing] {


    override def map[U](f: (Vector[WithSource[TToken]], WithSource[Nothing]) => (Vector[WithSource[TToken]], WithSource[U])): GrammarResultComplete[TToken, TSyntaxError, TLabel, U] = this

    override def flatMap[U](f: (Vector[WithSource[TToken]], WithSource[Nothing]) => GrammarResult[TToken, TSyntaxError, TLabel, U]): GrammarResult[TToken, TSyntaxError, TLabel, U] = this
    override def flatMap[U](f: (Vector[WithSource[TToken]], WithSource[Nothing]) => GrammarResultComplete[TToken, TSyntaxError, TLabel, U]): GrammarResultComplete[TToken, TSyntaxError, TLabel, U] = this

    override def treatFailureAsError: GrammarResultComplete[TToken, TSyntaxError, TLabel, Nothing] = GrammarResultError(failure)

    override def recoverFailure[U >: Nothing](f: (Vector[WithSource[TToken]], GrammarResultFailure[TToken, TSyntaxError, TLabel]) => GrammarResult[TToken, TSyntaxError, TLabel, U]): GrammarResult[TToken, TSyntaxError, TLabel, U] =
      f(Vector.empty, this)

    override def toEither: Either[NonEmptyVector[TSyntaxError], Nothing] = Left(failure)
  }

  final case class GrammarResultError[TToken, TSyntaxError, TLabel <: RuleLabel](error: NonEmptyVector[TSyntaxError])
    extends GrammarResultComplete[TToken, TSyntaxError, TLabel, Nothing]
      with GrammarResultNonSuccess[TToken, TSyntaxError, TLabel, Nothing] {


    override def map[U](f: (Vector[WithSource[TToken]], WithSource[Nothing]) => (Vector[WithSource[TToken]], WithSource[U])): GrammarResultComplete[TToken, TSyntaxError, TLabel, U] = this

    override def flatMap[U](f: (Vector[WithSource[TToken]], WithSource[Nothing]) => GrammarResult[TToken, TSyntaxError, TLabel, U]): GrammarResult[TToken, TSyntaxError, TLabel, U] = this
    override def flatMap[U](f: (Vector[WithSource[TToken]], WithSource[Nothing]) => GrammarResultComplete[TToken, TSyntaxError, TLabel, U]): GrammarResultComplete[TToken, TSyntaxError, TLabel, U] = this

    override def treatFailureAsError: GrammarResultComplete[TToken, TSyntaxError, TLabel, Nothing] = this

    override def recoverFailure[U >: Nothing](f: (Vector[WithSource[TToken]], GrammarResultFailure[TToken, TSyntaxError, TLabel]) => GrammarResult[TToken, TSyntaxError, TLabel, U]): GrammarResult[TToken, TSyntaxError, TLabel, U] =
      this

    override def toEither: Either[NonEmptyVector[TSyntaxError], Nothing] = Left(error)
  }

  sealed trait GrammarResultSuspend[TToken, TSyntaxError, TLabel <: RuleLabel, +T] extends GrammarResultNonSuccess[TToken, TSyntaxError, TLabel, T] {

    def continue(tokens: NonEmptyVector[WithSource[TToken]]): GrammarResult[TToken, TSyntaxError, TLabel, T]

    override def map[U](f: (Vector[WithSource[TToken]], WithSource[T]) => (Vector[WithSource[TToken]], WithSource[U])): GrammarResult[TToken, TSyntaxError, TLabel, U] =
      transformComplete(_.map(f))

    override def flatMap[U](f: (Vector[WithSource[TToken]], WithSource[T]) => GrammarResult[TToken, TSyntaxError, TLabel, U]): GrammarResult[TToken, TSyntaxError, TLabel, U] =
      transformComplete(_.flatMap(f))

    override def treatFailureAsError: GrammarResult[TToken, TSyntaxError, TLabel, T] =
      transformComplete(_.treatFailureAsError)


    override def recoverFailure[U >: T](f: (Vector[WithSource[TToken]], GrammarResultFailure[TToken, TSyntaxError, TLabel]) => GrammarResult[TToken, TSyntaxError, TLabel, U]): GrammarResult[TToken, TSyntaxError, TLabel, U] =
      new GrammarResultSuspend[TToken, TSyntaxError, TLabel, U] {
        override def continue(tokens: NonEmptyVector[WithSource[TToken]]): GrammarResult[TToken, TSyntaxError, TLabel, U] =
          GrammarResultSuspend.this.continue(tokens).recoverFailure { (tokens2, failure) => f(tokens.toVector ++ tokens2, failure) }

        override def completeResult(pos: FilePosition): GrammarResultComplete[TToken, TSyntaxError, TLabel, U] =
          GrammarResultSuspend.this.completeResult(pos).recoverFailure(f).completeResult(pos)
      }


    override def transformComplete[U](f: GrammarResultComplete[TToken, TSyntaxError, TLabel, T] => GrammarResult[TToken, TSyntaxError, TLabel, U]): GrammarResult[TToken, TSyntaxError, TLabel, U] =
      new GrammarResultSuspend[TToken, TSyntaxError, TLabel, U] {
        override def continue(tokens: NonEmptyVector[WithSource[TToken]]): GrammarResult[TToken, TSyntaxError, TLabel, U] =
          GrammarResultSuspend.this.continue(tokens).transformComplete(f)

        override def completeResult(pos: FilePosition): GrammarResultComplete[TToken, TSyntaxError, TLabel, U] =
          GrammarResultSuspend.this.completeResult(pos).transformComplete(f).completeResult(pos)
      }


  }

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

      def discard: Grammar[TToken, TSyntaxError, TLabel, Unit] = --> { _ => () }
      def ? (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError]): Grammar[TToken, TSyntaxError, TLabel, Option[T]] =
        --> (Some.apply) | EmptyStrGrammar(None)

      def +~
      (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
      : Grammar[TToken, TSyntaxError, TLabel, NonEmptyVector[T]] = {
        lazy val grammar1Cached = grammar1
        grammar1Cached ++ (grammar1Cached*) --> { case (head, tail) => NonEmptyVector(head, tail) }
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

  def parseAll[F[-_, +_, +_], TToken, TSyntaxError, TLabel <: RuleLabel, T]
  (factory: GrammarFactory[TToken, TSyntaxError, TLabel])
  (label: TLabel { type RuleType = T })
  (implicit monadError: MonadError[F[Any, NonEmptyVector[TSyntaxError], ?], NonEmptyVector[TSyntaxError]])
  : StreamTransformation[F, Any, NonEmptyVector[TSyntaxError], WithSource[TToken], FilePosition, T, FilePosition] =
    new StreamTransformation.Pure[F, NonEmptyVector[TSyntaxError], WithSource[TToken], FilePosition, T, FilePosition] {

      private val defaultParseOptions: ParseOptions[TToken, TSyntaxError, TLabel] = ParseOptions(Set.empty, None, factory)
      private val rule = factory(label)

      trait ParseStep
      case object ParseStepReady extends ParseStep
      final case class ParseStepPartial(suspend: GrammarResultSuspend[TToken, TSyntaxError, TLabel, T]) extends ParseStep

      override type State = ParseStep


      override def initialPure: ParseStep = ParseStepReady

      override def stepPure(s: ParseStep, ca: NonEmptyVector[WithSource[TToken]]): StepPure[ParseStep, NonEmptyVector[TSyntaxError], WithSource[TToken], T, FilePosition] =
        runParseStep(s, ca)  match {
          case GrammarResultSuccess(remaining, WithSource(value, _)) => Step.Produce(ParseStepReady, value, remaining)
          case GrammarResultFailure(failure) => Step.Fail(failure)
          case GrammarResultError(error) => Step.Fail(error)
          case suspend: GrammarResultSuspend[TToken, TSyntaxError, TLabel, T] => Step.Continue(ParseStepPartial(suspend))
        }



      override def endPure(s: ParseStep, end: FilePosition): (Vector[T], Either[NonEmptyVector[TSyntaxError], FilePosition]) =
        s match {
          case ParseStepReady => (Vector.empty, Right(end))
          case ParseStepPartial(suspend) => parseEnd(end, suspend.completeResult(end), Vector.empty)
        }


      private def runParseStep[A2 <: WithSource[TToken]](s: ParseStep, tokens: NonEmptyVector[A2]): GrammarResult[TToken, TSyntaxError, TLabel, T] =
        s match {
          case ParseStepReady => rule.parseTokens(tokens, defaultParseOptions)
          case ParseStepPartial(suspend) => suspend.continue(tokens)
        }

      private def parseEnd(end: FilePosition, result: GrammarResultComplete[TToken, TSyntaxError, TLabel, T], acc: Vector[T]): (Vector[T], Either[NonEmptyVector[TSyntaxError], FilePosition]) =
        result match {
          case GrammarResultSuccess(remaining, WithSource(value, _)) =>
            NonEmptyVector.fromVector(remaining) match {
              case Some(tokens) => parseEnd(end, rule.parseTokens(tokens, defaultParseOptions).completeResult(end), acc :+ value)
              case None => (acc :+ value, Right(end))
            }

          case GrammarResultFailure(failure) => (acc, Left(failure))
          case GrammarResultError(error) => (acc, Left(error))
        }
    }


  trait ErrorFactory[-TToken, -TTokenCategory, TSyntaxError] {
    def createError(error: GrammarError[TToken, TTokenCategory]): TSyntaxError
    def createAmbiguityError(location: SourceLocation): TSyntaxError
    def errorEndLocationOrder: Order[TSyntaxError]
  }

  type TokenMatcherFunc[TToken, T] = WithSource[TToken] => Option[WithSource[T]]

  private final case class RejectGrammar[TToken, TSyntaxError, TLabel <: RuleLabel, T](grammarErrors: NonEmptyVector[TSyntaxError]) extends Grammar[TToken, TSyntaxError, TLabel, T] {

    override def parseTokens(tokens: NonEmptyVector[WithSource[TToken]], options: TParseOptions): GrammarResult[TToken, TSyntaxError, TLabel, T] =
      GrammarResultFailure(grammarErrors)

    override def parseEnd(pos: FilePosition, options: TParseOptions): GrammarResultComplete[TToken, TSyntaxError, TLabel, T] =
      GrammarResultFailure(grammarErrors)

  }

  private final case class EmptyStrGrammar[TToken, TSyntaxError, TLabel <: RuleLabel, T](result: T) extends Grammar[TToken, TSyntaxError, TLabel, T] {

    override def parseTokens(tokens: NonEmptyVector[WithSource[TToken]], options: TParseOptions): GrammarResult[TToken, TSyntaxError, TLabel, T] =
      GrammarResultSuccess(tokens.toVector, WithSource(result, SourceLocation(tokens.head.location.start, tokens.head.location.start)))

    override def parseEnd(pos: FilePosition, options: TParseOptions): GrammarResultComplete[TToken, TSyntaxError, TLabel, T] =
      GrammarResultSuccess(Vector(), WithSource(result, SourceLocation(pos, pos)))

  }

  private final case class TokenGrammar[TToken, TSyntaxError, TLabel <: RuleLabel, TTokenCategory, T]
  (
    category: TTokenCategory,
    tokenMatcher: TokenMatcher[TToken, T]
  )(implicit
    errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError]
  ) extends Grammar[TToken, TSyntaxError, TLabel, T] {


    override def parseTokens(tokens: NonEmptyVector[WithSource[TToken]], options: TParseOptions): GrammarResult[TToken, TSyntaxError, TLabel, T] = {

      object MatchingToken {
        def unapply(arg: TToken): Option[T] = tokenMatcher.matchToken(arg)
      }

      tokens match {
        case NonEmptyVector(WithSource(token @ MatchingToken(value), loc), tail) =>
          GrammarResultSuccess(
            tail,
            WithSource(value, loc)
          )
        case NonEmptyVector(token, _) => GrammarResultFailure(NonEmptyVector.of(errorFactory.createError(GrammarError.UnexpectedToken(category, token))))
      }
    }

    override def parseEnd(pos: FilePosition, options: TParseOptions): GrammarResultComplete[TToken, TSyntaxError, TLabel, T] =
      GrammarResultFailure(NonEmptyVector.of(errorFactory.createError(GrammarError.UnexpectedEndOfFile(category, pos))))

  }

  private final class ConcatGrammar[TToken, TSyntaxError, TLabel <: RuleLabel, A, B, T]
  (
    grammarAUncached: => Grammar[TToken, TSyntaxError, TLabel, A],
    grammarBUncached: => Grammar[TToken, TSyntaxError, TLabel, B],
    combine: (WithSource[A], WithSource[B]) => WithSource[T]
  ) extends Grammar[TToken, TSyntaxError, TLabel, T] {

    private lazy val grammarA = grammarAUncached
    private lazy val grammarB = grammarBUncached

    private def nextHandler(valueA: WithSource[A], options: TParseOptions): GrammarResultSuspend[TToken, TSyntaxError, TLabel, T] =
      new GrammarResultSuspend[TToken, TSyntaxError, TLabel, T] {
        override def continue(tokens: NonEmptyVector[WithSource[TToken]]): GrammarResult[TToken, TSyntaxError, TLabel, T] =
          grammarB.parseTokens(tokens, options.notLeftRec).map {
            case (tokens3, valueB) =>
              (tokens3, combine(valueA, valueB))
          }

        override def completeResult(pos: FilePosition): GrammarResultComplete[TToken, TSyntaxError, TLabel, T] =
          grammarB.parseEnd(pos, options.notLeftRec).map {
            case (state3, valueB) =>
              (state3, combine(valueA, valueB))
          }

      }

    override def parseTokens(tokens: NonEmptyVector[WithSource[TToken]], options: TParseOptions): GrammarResult[TToken, TSyntaxError, TLabel, T] =
      grammarA.parseTokens(tokens, options).flatMap {
        case (nextHead +: nextTail, valueA) =>
          nextHandler(valueA, options).continue(NonEmptyVector(nextHead, nextTail))

        case (Vector(), valueA) =>
          nextHandler(valueA, options)
      }

    override def parseEnd(pos: FilePosition, options: TParseOptions): GrammarResultComplete[TToken, TSyntaxError, TLabel, T] =
      grammarA.parseEnd(pos, options).flatMap {
        case (Vector(), valueA) =>
          nextHandler(valueA, options).completeResult(pos)

        case (tokenHead +: tokenTail, valueA) =>
          nextHandler(valueA, options).continue(NonEmptyVector(tokenHead, tokenTail)).completeResult(pos)
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

    override def parseTokens(tokens: NonEmptyVector[WithSource[TToken]], options: TParseOptions): GrammarResult[TToken, TSyntaxError, TLabel, T] =
      inner.parseTokens(tokens, options).treatFailureAsError

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

  private final class ParseStateGrammar[TToken, TSyntaxError, TLabel <: RuleLabel, T]
  (
    innerUncached: => Grammar[TToken, TSyntaxError, TLabel, T],
    prevTokens: Vector[WithSource[TToken]],
    prevOptions: ParseOptions[TToken, TSyntaxError, TLabel]
  ) extends Grammar[TToken, TSyntaxError, TLabel, T] {

    private lazy val inner = innerUncached

    override def parseTokens(tokens: NonEmptyVector[WithSource[TToken]], options: TParseOptions): GrammarResult[TToken, TSyntaxError, TLabel, T] =
      inner.parseTokens(NonEmptyVector.fromVector(prevTokens).map { _ ++: tokens }.getOrElse(tokens), prevOptions)

    override def parseEnd(pos: FilePosition, options: TParseOptions): GrammarResultComplete[TToken, TSyntaxError, TLabel, T] =
      prevTokens match {
        case headToken +: tailToken => inner.parseTokens(NonEmptyVector(headToken, tailToken), prevOptions).completeResult(pos)
        case Vector() => inner.parseEnd(pos, prevOptions)
      }

  }

  private final class UnionGrammar[TToken, TSyntaxError, TLabel <: RuleLabel, T]
  (grammarAUncached: => Grammar[TToken, TSyntaxError, TLabel, T], grammarBUncached: => Grammar[TToken, TSyntaxError, TLabel, T])
  (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
    extends Grammar[TToken, TSyntaxError, TLabel, T] {

    private lazy val grammarA = grammarAUncached
    private lazy val grammarB = grammarBUncached

    override def parseTokens(tokens: NonEmptyVector[WithSource[TToken]], options: TParseOptions): GrammarResult[TToken, TSyntaxError, TLabel, T] =
      grammarA.parseTokens(tokens, options).recoverFailure {
        case (laterTokens, GrammarResultFailure(errorListA)) =>
          grammarB.parseTokens(tokens ++ laterTokens, options).transformComplete {
            case result @ GrammarResultSuccess(_, _) => result
            case result @ GrammarResultError(_) => result
            case GrammarResultFailure(errorListB) =>

              def findLastErrorPos(errorList: TErrorList): TSyntaxError =
                errorList.maximum(errorFactory.errorEndLocationOrder)


              GrammarResultFailure(
                errorFactory.errorEndLocationOrder.compare(findLastErrorPos(errorListA), findLastErrorPos(errorListB)) match {
                  case o if o > 0 => errorListA
                  case o if o < 0 => errorListB
                  case _ => errorListA ++: errorListB
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
                errorList.maximum(errorFactory.errorEndLocationOrder)


              GrammarResultFailure(
                errorFactory.errorEndLocationOrder.compare(findLastErrorPos(errorListA), findLastErrorPos(errorListB)) match {
                  case o if o > 0 => errorListA
                  case o if o < 0 => errorListB
                  case _ => errorListA ++: errorListB
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
    (grammars: NonEmptyVector[Lazy[Grammar[TToken, TSyntaxError, TLabel, T]]])
    (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
    : Grammar[TToken, TSyntaxError, TLabel, T] =
      grammars.tail match {
        case head2 +: tail =>
          tail.foldLeft(apply(grammars.head.value, head2.value)) { (a, b) => apply(a, b.value) }

        case Vector() =>
          grammars.head.value
      }

  }

  private final class RepeatGrammar[TToken, TSyntaxError, TLabel <: RuleLabel, T]
  (innerUncached: => Grammar[TToken, TSyntaxError, TLabel, T])
  (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
    extends Grammar[TToken, TSyntaxError, TLabel, Vector[T]] {

    private lazy val inner = innerUncached


    override def parseTokens(tokens: NonEmptyVector[WithSource[TToken]], options: TParseOptions): GrammarResult[TToken, TSyntaxError, TLabel, Vector[T]] =
      parseInner(tokens, options, Vector.empty)

    private def itemsLocation(pos: FilePosition, items: Vector[WithSource[T]]): SourceLocation =
      items match {
        case WithSource(_, SourceLocation(start, _)) +: _ :+ WithSource(_, SourceLocation(_, end)) => SourceLocation(start, end)
        case Vector(WithSource(_, loc)) => loc
        case Vector() => SourceLocation(pos, pos)
      }

    private def finalItems(items: Vector[WithSource[T]], pos: FilePosition): WithSource[Vector[T]] =
      WithSource(items.map(removeSource), itemsLocation(pos, items))

    private def parseInner(tokens: NonEmptyVector[WithSource[TToken]], options: TParseOptions, items: Vector[WithSource[T]]): GrammarResult[TToken, TSyntaxError, TLabel, Vector[T]] =
      inner.parseTokens(tokens, options).flatMap {
        case (Vector(), item) =>
          new GrammarResultSuspend[TToken, TSyntaxError, TLabel, Vector[T]] {
            override def continue(tokens: NonEmptyVector[WithSource[TToken]]): GrammarResult[TToken, TSyntaxError, TLabel, Vector[T]] =
              parseInner(tokens, options.notLeftRec, items :+ item)

            override def completeResult(pos: FilePosition): GrammarResultComplete[TToken, TSyntaxError, TLabel, Vector[T]] =
              GrammarResultSuccess(Vector(), finalItems(items :+ item, pos))
          }

        case (head +: tail, item) =>
          parseInner(NonEmptyVector(head, tail), options.notLeftRec, items :+ item)
      }
      .recoverFailure { (laterTokens, _) => GrammarResultSuccess(tokens.toVector ++ laterTokens, finalItems(items, tokens.head.location.start)) }

    override def parseEnd(pos: FilePosition, options: TParseOptions): GrammarResultComplete[TToken, TSyntaxError, TLabel, Vector[T]] =
      GrammarResultSuccess(Vector(), WithSource(Vector.empty, SourceLocation(pos, pos)))

    private def removeSource[A](ws: WithSource[A]): A = ws.value
  }


  private final class MapGrammar[TToken, TSyntaxError, TLabel <: RuleLabel, T, U](innerUncached: => Grammar[TToken, TSyntaxError, TLabel, T], f: GrammarResult[TToken, TSyntaxError, TLabel, T] => GrammarResult[TToken, TSyntaxError, TLabel, U]) extends Grammar[TToken, TSyntaxError, TLabel, U] {

    private lazy val inner = innerUncached


    override def parseTokens(tokens: NonEmptyVector[WithSource[TToken]], options: TParseOptions): GrammarResult[TToken, TSyntaxError, TLabel, U] =
      f(inner.parseTokens(tokens, options))

    override def parseEnd(pos: FilePosition, options: TParseOptions): GrammarResultComplete[TToken, TSyntaxError, TLabel, U] =
      f(inner.parseEnd(pos, options)).completeResult(pos)

  }

  private final class LabelRefGrammar[TToken, TSyntaxError, TLabel <: RuleLabel, T](label: TLabel { type RuleType = T }) extends Grammar[TToken, TSyntaxError, TLabel, T] {
    override def parseTokens(tokens: NonEmptyVector[WithSource[TToken]], options: TParseOptions): GrammarResult[TToken, TSyntaxError, TLabel, T] =
      options.factory(label).parseTokens(tokens, options)

    override def parseEnd(pos: FilePosition, options: TParseOptions): GrammarResultComplete[TToken, TSyntaxError, TLabel, T] =
      options.factory(label).parseEnd(pos, options)
  }

}
