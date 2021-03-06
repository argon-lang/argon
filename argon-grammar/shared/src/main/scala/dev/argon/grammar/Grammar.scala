package dev.argon.grammar

import dev.argon.grammar.Grammar._

import scala.collection.immutable.{Stream => _, _}
import scala.language.postfixOps
import cats._
import cats.data._
import cats.implicits._
import dev.argon.stream.StreamTransformation
import dev.argon.util._
import zio.{Chunk, IO, NonEmptyChunk, ZIO}

sealed trait Grammar[TToken, TSyntaxError, TLabel[_], +T] {

  type TErrorList = NonEmptyVector[TSyntaxError]
  type TParseOptions = ParseOptions[TToken, TSyntaxError, TLabel]

  def parseTokens(tokens: NonEmptyVector[WithSource[TToken]], options: TParseOptions): GrammarResult[TToken, TSyntaxError, TLabel, T]
  def parseEnd(pos: FilePosition, options: TParseOptions): GrammarResultComplete[TToken, TSyntaxError, TLabel, T]

}

object Grammar {

  abstract class GrammarFactory[TToken, TSyntaxError, TLabel[_]] {
    type TGrammar[+T] = Grammar[TToken, TSyntaxError, TLabel, T]

    @SuppressWarnings(Array("scalafix:DisableSyntax.var"))
    private var cache: Map[Any, AnyRef] = Map.empty

    protected def createGrammar[T](label: TLabel[T]): TGrammar[T]

    @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
    final def apply[T](label: TLabel[T]): TGrammar[T] = {
      cache.get(label) match {
        case Some(value) => value.asInstanceOf[TGrammar[T]]
        case None =>
          val result = createGrammar(label)
          cache = cache.updated(label, result)
          result
      }
    }

    def rule[T](label: TLabel[T]): Grammar[TToken, TSyntaxError, TLabel, T] =
      new LabelRefGrammar[TToken, TSyntaxError, TLabel, T](label)
  }

  sealed trait GrammarResult[TToken, TSyntaxError, TLabel[_], +T] {
    def map[U](f: (Vector[WithSource[TToken]], WithSource[T]) => (Vector[WithSource[TToken]], WithSource[U])): GrammarResult[TToken, TSyntaxError, TLabel, U]
    def flatMap[U](f: (Vector[WithSource[TToken]], WithSource[T]) => GrammarResult[TToken, TSyntaxError, TLabel, U]): GrammarResult[TToken, TSyntaxError, TLabel, U]

    def treatFailureAsError: GrammarResult[TToken, TSyntaxError, TLabel, T]

    def recoverFailure[U >: T](f: (Vector[WithSource[TToken]], GrammarResultFailure[TToken, TSyntaxError, TLabel]) => GrammarResult[TToken, TSyntaxError, TLabel, U]): GrammarResult[TToken, TSyntaxError, TLabel, U]

    def transformComplete[U](f: GrammarResultComplete[TToken, TSyntaxError, TLabel, T] => GrammarResult[TToken, TSyntaxError, TLabel, U]): GrammarResult[TToken, TSyntaxError, TLabel, U]
    def completeResult(pos: FilePosition): GrammarResultComplete[TToken, TSyntaxError, TLabel, T]
  }

  sealed trait GrammarResultNonSuccess[TToken, TSyntaxError, TLabel[_], +T] extends GrammarResult[TToken, TSyntaxError, TLabel, T]

  sealed trait GrammarResultComplete[TToken, TSyntaxError, TLabel[_], +T] extends GrammarResult[TToken, TSyntaxError, TLabel, T] {
    def toEither: Either[NonEmptyVector[TSyntaxError], (Vector[WithSource[TToken]], WithSource[T])]

    def flatMap[U](f: (Vector[WithSource[TToken]], WithSource[T]) => GrammarResultComplete[TToken, TSyntaxError, TLabel, U]): GrammarResultComplete[TToken, TSyntaxError, TLabel, U]


    override def map[U](f: (Vector[WithSource[TToken]], WithSource[T]) => (Vector[WithSource[TToken]], WithSource[U])): GrammarResultComplete[TToken, TSyntaxError, TLabel, U]
    override def flatMap[U](f: (Vector[WithSource[TToken]], WithSource[T]) => GrammarResult[TToken, TSyntaxError, TLabel, U]): GrammarResult[TToken, TSyntaxError, TLabel, U]
    override def treatFailureAsError: GrammarResultComplete[TToken, TSyntaxError, TLabel, T]

    override def transformComplete[U](f: GrammarResultComplete[TToken, TSyntaxError, TLabel, T] => GrammarResult[TToken, TSyntaxError, TLabel, U]): GrammarResult[TToken, TSyntaxError, TLabel, U] =
      f(this)

    override def completeResult(pos: FilePosition): GrammarResultComplete[TToken, TSyntaxError, TLabel, T] = this
  }

  final case class GrammarResultSuccess[TToken, TSyntaxError, TLabel[_], +T](tokens: Vector[WithSource[TToken]], value: WithSource[T]) extends GrammarResultComplete[TToken, TSyntaxError, TLabel, T] {
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

  final case class GrammarResultFailure[TToken, TSyntaxError, TLabel[_]](failure: NonEmptyVector[TSyntaxError])
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

  final case class GrammarResultError[TToken, TSyntaxError, TLabel[_]](error: NonEmptyVector[TSyntaxError])
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

  sealed trait GrammarResultSuspend[TToken, TSyntaxError, TLabel[_], +T] extends GrammarResultNonSuccess[TToken, TSyntaxError, TLabel, T] {

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

  sealed trait ParseOptions[TToken, TSyntaxError, TLabel[_]] {
    val factory: GrammarFactory[TToken, TSyntaxError, TLabel]

    def notLeftRec: ParseOptions[TToken, TSyntaxError, TLabel]
    def addLeftRec(rule: Grammar[TToken, TSyntaxError, TLabel, _]): ParseOptions[TToken, TSyntaxError, TLabel]
    def setLabel[T](label: TLabel[T]): ParseOptions[TToken, TSyntaxError, TLabel]
  }

  object ParseOptions {
    def apply[TToken, TSyntaxError, TLabel[_], T](leftRecRules: Set[Grammar[TToken, TSyntaxError, TLabel, _]], currentLabel: Option[TLabel[T]], factory: GrammarFactory[TToken, TSyntaxError, TLabel]): ParseOptions[TToken, TSyntaxError, TLabel] = {
      val factory2 = factory

      new ParseOptions[TToken, TSyntaxError, TLabel] {
        override val factory: GrammarFactory[TToken, TSyntaxError, TLabel] = factory2

        override def notLeftRec: ParseOptions[TToken, TSyntaxError, TLabel] =
          if(leftRecRules.isEmpty)
            this
          else
            ParseOptions(leftRecRules = Set.empty, currentLabel = currentLabel, factory = factory)

        override def addLeftRec(rule: Grammar[TToken, TSyntaxError, TLabel, _]): ParseOptions[TToken, TSyntaxError, TLabel] =
          ParseOptions(leftRecRules = leftRecRules + rule, currentLabel = currentLabel, factory = factory)

        override def setLabel[T2](label: TLabel[T2]): ParseOptions[TToken, TSyntaxError, TLabel] =
          ParseOptions(leftRecRules = leftRecRules, currentLabel = Some(label), factory = factory)
      }
    }
  }

  trait CombinerBase {

    trait GrammarConcatCombiner[A, B, T] {
      def combine(a: A, b: B): T
    }


    implicit def valueConcatCombiner[A, B]: GrammarConcatCombiner[A, B, (A, B)] = (a, b) => (a, b)
  }

  object Operators extends CombinerBase {

    final implicit class GrammarOperatorsImpl[TToken, TSyntaxError, TLabel[_], T](grammar1: => Grammar[TToken, TSyntaxError, TLabel, T]) {

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
      (implicit combiner: GrammarConcatCombiner[T, U, V])
      : Grammar[TToken, TSyntaxError, TLabel, V] =
        ConcatGrammar(grammar1, grammar2) { (a, b) => WithSource(combiner.combine(a.value, b.value), SourceLocation.merge(a.location, b.location)) }

      def ++! [U, V]
      (grammar2: => Grammar[TToken, TSyntaxError, TLabel, U])
      (implicit combiner: GrammarConcatCombiner[T, U, V])
      : Grammar[TToken, TSyntaxError, TLabel, V] =
        ConcatGrammar(grammar1, StrictGrammar(grammar2)) { (a, b) => WithSource(combiner.combine(a.value, b.value), SourceLocation.merge(a.location, b.location)) }

      def discard: Grammar[TToken, TSyntaxError, TLabel, Unit] = --> { _ => () }
      def ? (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError]): Grammar[TToken, TSyntaxError, TLabel, Option[T]] =
        --> (Some.apply) | EmptyStrGrammar(None)

      def +~
      : Grammar[TToken, TSyntaxError, TLabel, NonEmptyVector[T]] = {
        lazy val grammar1Cached = grammar1
        grammar1Cached ++ (grammar1Cached*) --> { case (head, tail) => NonEmptyVector(head, tail) }
      }

      def *
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


  def token[TToken, TSyntaxError, TLabel[_], TTokenCategory]
  (category: TTokenCategory, tokenMatches: TToken => Boolean)
  (implicit errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError])
  : Grammar[TToken, TSyntaxError, TLabel, TToken] =
    matcher(category, (t: TToken) => Some(t).filter(tokenMatches))

  def eof[TToken, TSyntaxError, TLabel[_], TTokenCategory, TResult]
  (result: TResult)
  (implicit errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError])
  : Grammar[TToken, TSyntaxError, TLabel, TResult] =
    EndOfFileGrammar(result)

  def matcher[TToken, TSyntaxError, TLabel[_], TTokenCategory, Result]
  (category: TTokenCategory, tokenMatcher: TToken => Option[Result])
  (implicit errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError])
  : Grammar[TToken, TSyntaxError, TLabel, Result] =
    matcher(category, TokenMatcher.Anything(tokenMatcher))

  def partialMatcher[TToken, TSyntaxError, TLabel[_], TTokenCategory, Result]
  (category: TTokenCategory)
  (f: PartialFunction[TToken, Result])
  (implicit errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError])
  : Grammar[TToken, TSyntaxError, TLabel, Result] =
    matcher(category, f.lift)

  def matcher[TToken, TSyntaxError, TLabel[_], TTokenCategory, Result]
  (category: TTokenCategory, tokenMatcher: TokenMatcher[TToken, Result])
  (implicit errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError])
  : Grammar[TToken, TSyntaxError, TLabel, Result] =
    TokenGrammar(category, tokenMatcher)

  def reject[TToken, TSyntaxError, TLabel[_], TTokenCategory, TResult]
  (grammarErrors: NonEmptyVector[TSyntaxError])
  : Grammar[TToken, TSyntaxError, TLabel, TResult] =
    RejectGrammar(grammarErrors)

  def parseAll[TToken, TSyntaxError, TLabel[_], T]
  (factory: GrammarFactory[TToken, TSyntaxError, TLabel])
  (label: TLabel[T])
  : StreamTransformation[Any, TSyntaxError, WithSource[TToken], FilePosition, T, FilePosition] =
    new StreamTransformation[Any, TSyntaxError, WithSource[TToken], FilePosition, T, FilePosition] {
      sealed trait ParseStep
      case object ParseStepReady extends ParseStep
      final case class ParseStepPartial(suspend: GrammarResultSuspend[TToken, TSyntaxError, TLabel, T]) extends ParseStep


      override type TransformState = ParseStep

      override def start: ZIO[Any, TSyntaxError, ParseStep] = IO.succeed(ParseStepReady)

      private def nonEmptyChunkToVector[A](chunk: NonEmptyChunk[A]): NonEmptyVector[A] =
        NonEmptyVector.of(chunk.head, chunk.tail.toVector: _*)

      override def consume(s: ParseStep, tokens: NonEmptyChunk[WithSource[TToken]]): ZIO[Any, TSyntaxError, (ParseStep, Chunk[T])] = {

        def runParseStepMulti(s: ParseStep, tokens: NonEmptyVector[WithSource[TToken]], acc: Chunk[T]): IO[TSyntaxError, (ParseStep, Chunk[T])] =
          runParseStep(s, tokens) match {
            case GrammarResultSuccess(remaining, WithSource(value, _)) =>
              NonEmptyVector.fromVector(remaining) match {
                case Some(remaining) => runParseStepMulti(ParseStepReady, remaining, acc :+ value)
                case None => IO.succeed((ParseStepReady, acc :+ value))
              }

            case GrammarResultFailure(failure) => IO.halt(ZIOErrorUtil.multiCauseVector(failure))
            case GrammarResultError(error) => IO.halt(ZIOErrorUtil.multiCauseVector(error))
            case suspend: GrammarResultSuspend[TToken, TSyntaxError, TLabel, T] =>
              IO.succeed((ParseStepPartial(suspend), acc))
          }

        val tokensNEV = nonEmptyChunkToVector(tokens)
        runParseStepMulti(s, tokensNEV, Chunk.empty)
      }

      override def finish(state: ParseStep, end: FilePosition): ZIO[Any, TSyntaxError, (Chunk[T], FilePosition)] =
        state match {
          case ParseStepReady => IO.succeed((Chunk.empty, end))
          case ParseStepPartial(suspend) =>
            def parseEnd(result: GrammarResultComplete[TToken, TSyntaxError, TLabel, T], acc: Chunk[T]): IO[TSyntaxError, (Chunk[T], FilePosition)] =
              result match {
                case GrammarResultSuccess(remaining, WithSource(value, _)) =>
                  NonEmptyVector.fromVector(remaining) match {
                    case Some(tokens) => parseEnd(rule.parseTokens(tokens, defaultParseOptions).completeResult(end), acc :+ value)
                    case None => IO.succeed((acc :+ value, end))
                  }

                case GrammarResultFailure(failure) => IO.halt(ZIOErrorUtil.multiCauseVector(failure))
                case GrammarResultError(error) => IO.halt(ZIOErrorUtil.multiCauseVector(error))
              }

            parseEnd(suspend.completeResult(end), Chunk.empty)
        }

      private val defaultParseOptions: ParseOptions[TToken, TSyntaxError, TLabel] = ParseOptions(Set.empty, None, factory)
      private val rule = factory(label)

      private def runParseStep(s: ParseStep, tokens: NonEmptyVector[WithSource[TToken]]): GrammarResult[TToken, TSyntaxError, TLabel, T] =
        s match {
          case ParseStepReady => rule.parseTokens(tokens, defaultParseOptions)
          case ParseStepPartial(suspend) => suspend.continue(tokens)
        }
    }


  trait ErrorFactory[-TToken, -TTokenCategory, TSyntaxError] {
    def createError(error: GrammarError[TToken, TTokenCategory]): TSyntaxError
    def errorEndLocationOrder: Order[TSyntaxError]
  }

  type TokenMatcherFunc[TToken, T] = WithSource[TToken] => Option[WithSource[T]]

  private final case class RejectGrammar[TToken, TSyntaxError, TLabel[_], T](grammarErrors: NonEmptyVector[TSyntaxError]) extends Grammar[TToken, TSyntaxError, TLabel, T] {

    override def parseTokens(tokens: NonEmptyVector[WithSource[TToken]], options: TParseOptions): GrammarResult[TToken, TSyntaxError, TLabel, T] =
      GrammarResultFailure(grammarErrors)

    override def parseEnd(pos: FilePosition, options: TParseOptions): GrammarResultComplete[TToken, TSyntaxError, TLabel, T] =
      GrammarResultFailure(grammarErrors)

  }

  private final case class EmptyStrGrammar[TToken, TSyntaxError, TLabel[_], T](result: T) extends Grammar[TToken, TSyntaxError, TLabel, T] {

    override def parseTokens(tokens: NonEmptyVector[WithSource[TToken]], options: TParseOptions): GrammarResult[TToken, TSyntaxError, TLabel, T] =
      GrammarResultSuccess(tokens.toVector, WithSource(result, SourceLocation(tokens.head.location.start, tokens.head.location.start)))

    override def parseEnd(pos: FilePosition, options: TParseOptions): GrammarResultComplete[TToken, TSyntaxError, TLabel, T] =
      GrammarResultSuccess(Vector(), WithSource(result, SourceLocation(pos, pos)))

  }

  private final case class EndOfFileGrammar[TToken, TSyntaxError, TLabel[_], TTokenCategory, T]
  (
    result: T
  )(
    implicit errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError]
  ) extends Grammar[TToken, TSyntaxError, TLabel, T] {
    override def parseTokens(tokens: NonEmptyVector[WithSource[TToken]], options: TParseOptions): GrammarResult[TToken, TSyntaxError, TLabel, T] =
      GrammarResultFailure(NonEmptyVector.of(errorFactory.createError(GrammarError.ExpectedEndOfFile(tokens.head))))

    override def parseEnd(pos: FilePosition, options: TParseOptions): GrammarResultComplete[TToken, TSyntaxError, TLabel, T] =
      GrammarResultSuccess(Vector(), WithSource(result, SourceLocation(pos, pos)))
  }

  private final case class TokenGrammar[TToken, TSyntaxError, TLabel[_], TTokenCategory, T]
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
        case NonEmptyVector(WithSource(MatchingToken(value), loc), tail) =>
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

  private final class ConcatGrammar[TToken, TSyntaxError, TLabel[_], A, B, T]
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
        case (VectorUnCons(VectorUnCons.NonEmpty(nextHead, nextTail)), valueA) =>
          nextHandler(valueA, options).continue(NonEmptyVector(nextHead, nextTail))

        case (VectorUnCons(VectorUnCons.Empty), valueA) =>
          nextHandler(valueA, options)
      }

    override def parseEnd(pos: FilePosition, options: TParseOptions): GrammarResultComplete[TToken, TSyntaxError, TLabel, T] =
      grammarA.parseEnd(pos, options).flatMap[T] { (remaining: Vector[WithSource[TToken]], valueA: WithSource[A]) =>
        remaining match {
          case VectorUnCons(VectorUnCons.Empty) =>
            nextHandler(valueA, options).completeResult(pos)

          case VectorUnCons(VectorUnCons.NonEmpty(tokenHead, tokenTail)) =>
            nextHandler(valueA, options).continue(NonEmptyVector(tokenHead, tokenTail)).completeResult(pos)
        }
      }

  }

  private object ConcatGrammar {
    def apply[TToken, TSyntaxError, TLabel[_], A, B, T]
    (
      grammarA: => Grammar[TToken, TSyntaxError, TLabel, A],
      grammarB: => Grammar[TToken, TSyntaxError, TLabel, B]
    )(
      combine: (WithSource[A], WithSource[B]) => WithSource[T]
    ): ConcatGrammar[TToken, TSyntaxError, TLabel, A, B, T] =
      new ConcatGrammar(grammarA, grammarB, combine)

  }

  private final class StrictGrammar[TToken, TSyntaxError, TLabel[_], T]
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
    def apply[TToken, TSyntaxError, TLabel[_], T]
    (
      inner: => Grammar[TToken, TSyntaxError, TLabel, T]
    ): StrictGrammar[TToken, TSyntaxError, TLabel, T] =
      new StrictGrammar(inner)

  }

  private final class ParseStateGrammar[TToken, TSyntaxError, TLabel[_], T]
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
        case VectorUnCons(VectorUnCons.NonEmpty(headToken, tailToken)) =>
          inner.parseTokens(NonEmptyVector(headToken, tailToken), prevOptions).completeResult(pos)
        case VectorUnCons(VectorUnCons.Empty) => inner.parseEnd(pos, prevOptions)
      }

  }

  private final class UnionGrammar[TToken, TSyntaxError, TLabel[_], T]
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

    def apply[TToken, TSyntaxError, TLabel[_], T]
    (grammarA: => Grammar[TToken, TSyntaxError, TLabel, T], grammarB: => Grammar[TToken, TSyntaxError, TLabel, T])
    (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
    : Grammar[TToken, TSyntaxError, TLabel, T] =
      new UnionGrammar[TToken, TSyntaxError, TLabel, T](grammarA, grammarB)

    def fromList[TToken, TSyntaxError, TLabel[_], T]
    (grammars: NonEmptyVector[Lazy[Grammar[TToken, TSyntaxError, TLabel, T]]])
    (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
    : Grammar[TToken, TSyntaxError, TLabel, T] =
      grammars.tail match {
        case VectorUnCons(VectorUnCons.NonEmpty(head2, tail)) =>
          tail.foldLeft(apply(grammars.head.value, head2.value)) { (a, b) => apply(a, b.value) }

        case VectorUnCons(VectorUnCons.Empty) =>
          grammars.head.value
      }

  }

  private final class RepeatGrammar[TToken, TSyntaxError, TLabel[_], T]
  (innerUncached: => Grammar[TToken, TSyntaxError, TLabel, T])
    extends Grammar[TToken, TSyntaxError, TLabel, Vector[T]] {

    private lazy val inner = innerUncached


    override def parseTokens(tokens: NonEmptyVector[WithSource[TToken]], options: TParseOptions): GrammarResult[TToken, TSyntaxError, TLabel, Vector[T]] =
      parseInner(tokens, options, Vector.empty)

    private def itemsLocation(pos: FilePosition, items: Vector[WithSource[T]]): SourceLocation =
      items match {
        case VectorUnCons(VectorUnCons.NonEmpty(WithSource(_, SourceLocation(start, _)), VectorUnCons.Rev(VectorUnCons.Rev.NonEmpty(_, WithSource(_, SourceLocation(_, end)))))) =>
          SourceLocation(start, end)

        case VectorUnCons(VectorUnCons.NonEmpty(WithSource(_, loc), VectorUnCons.Rev(VectorUnCons.Rev.Empty))) =>
          loc

        case VectorUnCons(VectorUnCons.Empty) => SourceLocation(pos, pos)
      }

    private def finalItems(items: Vector[WithSource[T]], pos: FilePosition): WithSource[Vector[T]] =
      WithSource(items.map(removeSource), itemsLocation(pos, items))

    private def parseInner(tokens: NonEmptyVector[WithSource[TToken]], options: TParseOptions, items: Vector[WithSource[T]]): GrammarResult[TToken, TSyntaxError, TLabel, Vector[T]] =
      inner.parseTokens(tokens, options).flatMap {
        case (VectorUnCons(VectorUnCons.Empty), item) =>
          new GrammarResultSuspend[TToken, TSyntaxError, TLabel, Vector[T]] {
            override def continue(tokens: NonEmptyVector[WithSource[TToken]]): GrammarResult[TToken, TSyntaxError, TLabel, Vector[T]] =
              parseInner(tokens, options.notLeftRec, items :+ item)

            override def completeResult(pos: FilePosition): GrammarResultComplete[TToken, TSyntaxError, TLabel, Vector[T]] =
              GrammarResultSuccess(Vector(), finalItems(items :+ item, pos))
          }

        case (VectorUnCons(VectorUnCons.NonEmpty(head, tail)), item) =>
          parseInner(NonEmptyVector(head, tail), options.notLeftRec, items :+ item)
      }
      .recoverFailure { (laterTokens, _) => GrammarResultSuccess(tokens.toVector ++ laterTokens, finalItems(items, tokens.head.location.start)) }

    override def parseEnd(pos: FilePosition, options: TParseOptions): GrammarResultComplete[TToken, TSyntaxError, TLabel, Vector[T]] =
      GrammarResultSuccess(Vector(), WithSource(Vector.empty, SourceLocation(pos, pos)))

    private def removeSource[A](ws: WithSource[A]): A = ws.value
  }


  private final class MapGrammar[TToken, TSyntaxError, TLabel[_], T, U](innerUncached: => Grammar[TToken, TSyntaxError, TLabel, T], f: GrammarResult[TToken, TSyntaxError, TLabel, T] => GrammarResult[TToken, TSyntaxError, TLabel, U]) extends Grammar[TToken, TSyntaxError, TLabel, U] {

    private lazy val inner = innerUncached


    override def parseTokens(tokens: NonEmptyVector[WithSource[TToken]], options: TParseOptions): GrammarResult[TToken, TSyntaxError, TLabel, U] =
      f(inner.parseTokens(tokens, options))

    override def parseEnd(pos: FilePosition, options: TParseOptions): GrammarResultComplete[TToken, TSyntaxError, TLabel, U] =
      f(inner.parseEnd(pos, options)).completeResult(pos)

  }

  private final class LabelRefGrammar[TToken, TSyntaxError, TLabel[_], T](label: TLabel[T]) extends Grammar[TToken, TSyntaxError, TLabel, T] {
    override def parseTokens(tokens: NonEmptyVector[WithSource[TToken]], options: TParseOptions): GrammarResult[TToken, TSyntaxError, TLabel, T] =
      options.factory(label).parseTokens(tokens, options)

    override def parseEnd(pos: FilePosition, options: TParseOptions): GrammarResultComplete[TToken, TSyntaxError, TLabel, T] =
      options.factory(label).parseEnd(pos, options)
  }

  @SuppressWarnings(Array("scalafix:MissingFinal.trait"))
  trait EmbeddedGrammar[TSyntaxError, TTokenA, TLabelA[_], TTokenB, TLabelB[_], T] extends Grammar[TTokenA, TSyntaxError, TLabelA, T] {

    protected val outerGrammar: Grammar[TTokenA, TSyntaxError, TLabelA, TTokenB]
    protected val innerGrammar: Grammar[TTokenB, TSyntaxError, TLabelB, T]
    protected val innerFactory: GrammarFactory[TTokenB, TSyntaxError, TLabelB]
    def stopToken(token: TTokenB): Boolean
    def unexpectedEndOfFileError(pos: FilePosition): TSyntaxError
    def unexpectedToken(token: WithSource[TTokenB]): TSyntaxError

    final override def parseTokens(tokens: NonEmptyVector[WithSource[TTokenA]], options: TParseOptions): GrammarResult[TTokenA, TSyntaxError, TLabelA, T] = {

      def handleResult(result: GrammarResult[TTokenA, TSyntaxError, TLabelA, TTokenB], acc: Vector[WithSource[TTokenB]]): GrammarResult[TTokenA, TSyntaxError, TLabelA, T] =
        result match {
          case result: GrammarResultError[TTokenA, TSyntaxError, TLabelA] => result
          case result: GrammarResultFailure[TTokenA, TSyntaxError, TLabelA] => result
          case result: GrammarResultSuspend[TTokenA, TSyntaxError, TLabelA, TTokenB] =>
            new GrammarResultSuspend[TTokenA, TSyntaxError, TLabelA, T] {
              override def continue(tokens: NonEmptyVector[WithSource[TTokenA]]): GrammarResult[TTokenA, TSyntaxError, TLabelA, T] =
                handleResult(result.continue(tokens), acc)

              override def completeResult(pos: FilePosition): GrammarResultComplete[TTokenA, TSyntaxError, TLabelA, T] =
                handleResult(result.completeResult(pos), acc).completeResult(pos)
            }

          case GrammarResultSuccess(extra, tokenB) if stopToken(tokenB.value) =>
            def handleFinalResult(result: GrammarResultComplete[TTokenB, TSyntaxError, TLabelB, T]): GrammarResult[TTokenA, TSyntaxError, TLabelA, T] =
              result match {
                case GrammarResultFailure(failure) => GrammarResultFailure(failure)
                case GrammarResultError(error) => GrammarResultError(error)

                case GrammarResultSuccess(VectorUnCons(VectorUnCons.Empty), value) =>
                  GrammarResultSuccess(extra, value)

                case GrammarResultSuccess(VectorUnCons(VectorUnCons.NonEmpty(head, _)), _) =>
                  GrammarResultFailure(NonEmptyVector.of(unexpectedToken(head)))
              }

            NonEmptyVector.fromVector(acc) match {
              case Some(acc) =>
                innerGrammar.parseTokens(acc, ParseOptions(Set.empty, None, innerFactory)) match {
                  case GrammarResultError(errors) => GrammarResultError(errors)
                  case GrammarResultFailure(failure) => GrammarResultFailure(failure)
                  case innerResult: GrammarResultSuspend[TTokenB, TSyntaxError, TLabelB, T] =>
                    handleFinalResult(innerResult.completeResult(tokenB.location.start))

                  case GrammarResultSuccess(VectorUnCons(VectorUnCons.Empty), value) =>
                    GrammarResultSuccess(extra, value)

                  case GrammarResultSuccess(VectorUnCons(VectorUnCons.NonEmpty(head, _)), _) =>
                    GrammarResultFailure(NonEmptyVector.of(unexpectedToken(head)))
                }

              case None =>
                handleFinalResult(innerGrammar.parseEnd(tokenB.location.start, ParseOptions(Set.empty, None, innerFactory)))
            }


          case GrammarResultSuccess(VectorUnCons(VectorUnCons.Empty), tokenB) =>
            new GrammarResultSuspend[TTokenA, TSyntaxError, TLabelA, T] {
              override def continue(tokens: NonEmptyVector[WithSource[TTokenA]]): GrammarResult[TTokenA, TSyntaxError, TLabelA, T] =
                handleResult(outerGrammar.parseTokens(tokens, options), acc :+ tokenB)

              override def completeResult(pos: FilePosition): GrammarResultComplete[TTokenA, TSyntaxError, TLabelA, T] =
                GrammarResultFailure(NonEmptyVector.of(unexpectedEndOfFileError(pos)))
            }

          case GrammarResultSuccess(VectorUnCons(VectorUnCons.NonEmpty(head, tail)), tokenB) =>
            handleResult(outerGrammar.parseTokens(NonEmptyVector(head, tail), options), acc :+ tokenB)

        }

      handleResult(outerGrammar.parseTokens(tokens, options), Vector.empty)
    }

    final override def parseEnd(pos: FilePosition, options: TParseOptions): GrammarResultComplete[TTokenA, TSyntaxError, TLabelA, T] =
      GrammarResultFailure(NonEmptyVector.of(unexpectedEndOfFileError(pos)))

  }

}
