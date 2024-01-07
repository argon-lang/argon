package dev.argon.grammar

import dev.argon.grammar.Grammar.*
import scala.collection.immutable.{Stream as _, *}
import scala.language.postfixOps
import dev.argon.util.{*, given}
import zio.*
import zio.stream.*

sealed trait Grammar[TToken, TPosition, TSyntaxError, TLabel[_], +T] {

  type TErrorList = NonEmptyChunk[TSyntaxError]
  type TParseOptions = ParseOptions[TToken, TPosition, TSyntaxError, TLabel]

  def parseTokens(tokens: NonEmptyChunk[WithLocation[TToken, TPosition]], options: TParseOptions)
    : GrammarResult[TToken, TPosition, TSyntaxError, TLabel, T]

  def parseEnd(pos: TPosition, options: TParseOptions): GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, T]

}

object Grammar {

  abstract class GrammarFactory[TToken, TPosition, TSyntaxError, TLabel[_]] {
    type TGrammar[+T] = Grammar[TToken, TPosition, TSyntaxError, TLabel, T]

    val fileName: Option[String]

    @SuppressWarnings(Array("scalafix:DisableSyntax.var"))
    private var cache: Map[Any, AnyRef] = Map.empty

    protected def createGrammar[T](label: TLabel[T]): TGrammar[T]

    @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
    final def apply[T](label: TLabel[T]): TGrammar[T] =
      cache.get(label) match {
        case Some(value) => value.asInstanceOf[TGrammar[T]]
        case None =>
          val result = createGrammar(label)
          cache = cache.updated(label, result)
          result
      }

    def rule[T](label: TLabel[T]): Grammar[TToken, TPosition, TSyntaxError, TLabel, T] =
      new LabelRefGrammar[TToken, TPosition, TSyntaxError, TLabel, T](label)

  }

  sealed trait GrammarResult[TToken, TPosition, TSyntaxError, TLabel[_], +T] {

    def map[U](f: (Chunk[WithLocation[TToken, TPosition]], WithLocation[T, TPosition]) => (Chunk[WithLocation[TToken, TPosition]], WithLocation[U, TPosition]))
      : GrammarResult[TToken, TPosition, TSyntaxError, TLabel, U]

    def flatMap[U](f: (Chunk[WithLocation[TToken, TPosition]], WithLocation[T, TPosition]) => GrammarResult[TToken, TPosition, TSyntaxError, TLabel, U])
      : GrammarResult[TToken, TPosition, TSyntaxError, TLabel, U]

    def treatFailureAsError: GrammarResult[TToken, TPosition, TSyntaxError, TLabel, T]

    def recoverFailure[U >: T]
      (f: (Chunk[WithLocation[TToken, TPosition]], GrammarResultFailure[TToken, TPosition, TSyntaxError, TLabel]) => GrammarResult[
        TToken,
        TPosition,
        TSyntaxError,
        TLabel,
        U,
      ])
      : GrammarResult[TToken, TPosition, TSyntaxError, TLabel, U]

    def transformComplete[U]
      (f: GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, T] => GrammarResult[TToken, TPosition, TSyntaxError, TLabel, U])
      : GrammarResult[TToken, TPosition, TSyntaxError, TLabel, U]

    def completeResult(pos: TPosition): GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, T]
  }

  sealed trait GrammarResultNonSuccess[TToken, TPosition, TSyntaxError, TLabel[_], +T]
      extends GrammarResult[TToken, TPosition, TSyntaxError, TLabel, T]

  sealed trait GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel[_], +T]
      extends GrammarResult[TToken, TPosition, TSyntaxError, TLabel, T] {
    def toEither: Either[NonEmptyChunk[TSyntaxError], (Chunk[WithLocation[TToken, TPosition]], WithLocation[T, TPosition])]

    def flatMap[U]
      (f: (Chunk[WithLocation[TToken, TPosition]], WithLocation[T, TPosition]) => GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, U])
      : GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, U]

    override def map[U](f: (Chunk[WithLocation[TToken, TPosition]], WithLocation[T, TPosition]) => (Chunk[WithLocation[TToken, TPosition]], WithLocation[U, TPosition]))
      : GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, U]

    override def flatMap[U]
      (f: (Chunk[WithLocation[TToken, TPosition]], WithLocation[T, TPosition]) => GrammarResult[TToken, TPosition, TSyntaxError, TLabel, U])
      : GrammarResult[TToken, TPosition, TSyntaxError, TLabel, U]

    override def treatFailureAsError: GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, T]

    override def transformComplete[U]
      (f: GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, T] => GrammarResult[TToken, TPosition, TSyntaxError, TLabel, U])
      : GrammarResult[TToken, TPosition, TSyntaxError, TLabel, U] = f(this)

    override def completeResult(pos: TPosition): GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, T] = this
  }

  final case class GrammarResultSuccess[TToken, TPosition, TSyntaxError, TLabel[_], +T]
    (tokens: Chunk[WithLocation[TToken, TPosition]], value: WithLocation[T, TPosition])
      extends GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, T] {

    override def map[U](f: (Chunk[WithLocation[TToken, TPosition]], WithLocation[T, TPosition]) => (Chunk[WithLocation[TToken, TPosition]], WithLocation[U, TPosition]))
      : GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, U] =
      f(tokens, value) match { case (tokens2, value2) => GrammarResultSuccess(tokens2, value2) }

    override def flatMap[U]
      (f: (Chunk[WithLocation[TToken, TPosition]], WithLocation[T, TPosition]) => GrammarResult[TToken, TPosition, TSyntaxError, TLabel, U])
      : GrammarResult[TToken, TPosition, TSyntaxError, TLabel, U] = f(tokens, value)

    override def flatMap[U]
      (f: (Chunk[WithLocation[TToken, TPosition]], WithLocation[T, TPosition]) => GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, U])
      : GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, U] = f(tokens, value)

    override def treatFailureAsError: GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, T] = this

    override def recoverFailure[U >: T]
      (f: (Chunk[WithLocation[TToken, TPosition]], GrammarResultFailure[TToken, TPosition, TSyntaxError, TLabel]) => GrammarResult[
        TToken,
        TPosition,
        TSyntaxError,
        TLabel,
        U,
      ])
      : GrammarResult[TToken, TPosition, TSyntaxError, TLabel, U] = this

    override def toEither: Either[NonEmptyChunk[TSyntaxError], (Chunk[WithLocation[TToken, TPosition]], WithLocation[T, TPosition])] =
      Right((tokens, value))

  }

  final case class GrammarResultFailure[TToken, TPosition, TSyntaxError, TLabel[_]](failure: NonEmptyChunk[TSyntaxError])
      extends GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, Nothing]
      with GrammarResultNonSuccess[TToken, TPosition, TSyntaxError, TLabel, Nothing] {

    override def map[U]
      (f: (Chunk[WithLocation[TToken, TPosition]], WithLocation[Nothing, TPosition]) => (Chunk[WithLocation[TToken, TPosition]], WithLocation[U, TPosition]))
      : GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, U] = this

    override def flatMap[U]
      (f: (Chunk[WithLocation[TToken, TPosition]], WithLocation[Nothing, TPosition]) => GrammarResult[TToken, TPosition, TSyntaxError, TLabel, U])
      : GrammarResult[TToken, TPosition, TSyntaxError, TLabel, U] = this

    override def flatMap[U]
      (f: (Chunk[WithLocation[TToken, TPosition]], WithLocation[Nothing, TPosition]) => GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, U])
      : GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, U] = this

    override def treatFailureAsError: GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, Nothing] =
      GrammarResultError(failure)

    override def recoverFailure[U >: Nothing]
      (f: (Chunk[WithLocation[TToken, TPosition]], GrammarResultFailure[TToken, TPosition, TSyntaxError, TLabel]) => GrammarResult[
        TToken,
        TPosition,
        TSyntaxError,
        TLabel,
        U,
      ])
      : GrammarResult[TToken, TPosition, TSyntaxError, TLabel, U] = f(Chunk.empty, this)

    override def toEither: Either[NonEmptyChunk[TSyntaxError], Nothing] = Left(failure)
  }

  final case class GrammarResultError[TToken, TPosition, TSyntaxError, TLabel[_]](error: NonEmptyChunk[TSyntaxError])
      extends GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, Nothing]
      with GrammarResultNonSuccess[TToken, TPosition, TSyntaxError, TLabel, Nothing] {

    override def map[U]
      (f: (Chunk[WithLocation[TToken, TPosition]], WithLocation[Nothing, TPosition]) => (Chunk[WithLocation[TToken, TPosition]], WithLocation[U, TPosition]))
      : GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, U] = this

    override def flatMap[U]
      (f: (Chunk[WithLocation[TToken, TPosition]], WithLocation[Nothing, TPosition]) => GrammarResult[TToken, TPosition, TSyntaxError, TLabel, U])
      : GrammarResult[TToken, TPosition, TSyntaxError, TLabel, U] = this

    override def flatMap[U]
      (f: (Chunk[WithLocation[TToken, TPosition]], WithLocation[Nothing, TPosition]) => GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, U])
      : GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, U] = this

    override def treatFailureAsError: GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, Nothing] = this

    override def recoverFailure[U >: Nothing]
      (f: (Chunk[WithLocation[TToken, TPosition]], GrammarResultFailure[TToken, TPosition, TSyntaxError, TLabel]) => GrammarResult[
        TToken,
        TPosition,
        TSyntaxError,
        TLabel,
        U,
      ])
      : GrammarResult[TToken, TPosition, TSyntaxError, TLabel, U] = this

    override def toEither: Either[NonEmptyChunk[TSyntaxError], Nothing] = Left(error)
  }

  sealed trait GrammarResultSuspend[TToken, TPosition, TSyntaxError, TLabel[_], +T]
      extends GrammarResultNonSuccess[TToken, TPosition, TSyntaxError, TLabel, T] {

    def continue(tokens: NonEmptyChunk[WithLocation[TToken, TPosition]]): GrammarResult[TToken, TPosition, TSyntaxError, TLabel, T]

    override def map[U](f: (Chunk[WithLocation[TToken, TPosition]], WithLocation[T, TPosition]) => (Chunk[WithLocation[TToken, TPosition]], WithLocation[U, TPosition]))
      : GrammarResult[TToken, TPosition, TSyntaxError, TLabel, U] = transformComplete(_.map(f))

    override def flatMap[U]
      (f: (Chunk[WithLocation[TToken, TPosition]], WithLocation[T, TPosition]) => GrammarResult[TToken, TPosition, TSyntaxError, TLabel, U])
      : GrammarResult[TToken, TPosition, TSyntaxError, TLabel, U] = transformComplete(_.flatMap(f))

    override def treatFailureAsError: GrammarResult[TToken, TPosition, TSyntaxError, TLabel, T] =
      transformComplete(_.treatFailureAsError)

    override def recoverFailure[U >: T]
      (f: (Chunk[WithLocation[TToken, TPosition]], GrammarResultFailure[TToken, TPosition, TSyntaxError, TLabel]) => GrammarResult[
        TToken,
        TPosition,
        TSyntaxError,
        TLabel,
        U,
      ])
      : GrammarResult[TToken, TPosition, TSyntaxError, TLabel, U] =
      new GrammarResultSuspend[TToken, TPosition, TSyntaxError, TLabel, U] {

        override def continue(tokens: NonEmptyChunk[WithLocation[TToken, TPosition]])
          : GrammarResult[TToken, TPosition, TSyntaxError, TLabel, U] =
          GrammarResultSuspend.this.continue(tokens).recoverFailure { (tokens2, failure) =>
            f(tokens ++ tokens2, failure)
          }

        override def completeResult(pos: TPosition): GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, U] =
          GrammarResultSuspend.this.completeResult(pos).recoverFailure(f).completeResult(pos)

      }

    override def transformComplete[U]
      (f: GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, T] => GrammarResult[TToken, TPosition, TSyntaxError, TLabel, U])
      : GrammarResult[TToken, TPosition, TSyntaxError, TLabel, U] =
      new GrammarResultSuspend[TToken, TPosition, TSyntaxError, TLabel, U] {

        override def continue(tokens: NonEmptyChunk[WithLocation[TToken, TPosition]])
          : GrammarResult[TToken, TPosition, TSyntaxError, TLabel, U] =
          GrammarResultSuspend.this.continue(tokens).transformComplete(f)

        override def completeResult(pos: TPosition): GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, U] =
          GrammarResultSuspend.this.completeResult(pos).transformComplete(f).completeResult(pos)

      }

  }

  sealed trait ParseOptions[TToken, TPosition, TSyntaxError, TLabel[_]] {
    val factory: GrammarFactory[TToken, TPosition, TSyntaxError, TLabel]

    def notLeftRec: ParseOptions[TToken, TPosition, TSyntaxError, TLabel]
    def addLeftRec(rule: Grammar[TToken, TPosition, TSyntaxError, TLabel, ?]): ParseOptions[TToken, TPosition, TSyntaxError, TLabel]
    def setLabel[T](label: TLabel[T]): ParseOptions[TToken, TPosition, TSyntaxError, TLabel]
  }

  object ParseOptions {

    def apply[TToken, TPosition, TSyntaxError, TLabel[_], T]
      (
        leftRecRules: Set[Grammar[TToken, TPosition, TSyntaxError, TLabel, ?]],
        currentLabel: Option[TLabel[T]],
        factory: GrammarFactory[TToken, TPosition, TSyntaxError, TLabel],
      )
      : ParseOptions[TToken, TPosition, TSyntaxError, TLabel] = {
      val factory2 = factory

      new ParseOptions[TToken, TPosition, TSyntaxError, TLabel] {
        override val factory: GrammarFactory[TToken, TPosition, TSyntaxError, TLabel] = factory2

        override def notLeftRec: ParseOptions[TToken, TPosition, TSyntaxError, TLabel] =
          if leftRecRules.isEmpty then
            this
          else
            ParseOptions(leftRecRules = Set.empty, currentLabel = currentLabel, factory = factory)

        override def addLeftRec(rule: Grammar[TToken, TPosition, TSyntaxError, TLabel, ?])
          : ParseOptions[TToken, TPosition, TSyntaxError, TLabel] =
          ParseOptions(leftRecRules = leftRecRules + rule, currentLabel = currentLabel, factory = factory)

        override def setLabel[T2](label: TLabel[T2]): ParseOptions[TToken, TPosition, TSyntaxError, TLabel] =
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

    implicit final class GrammarOperatorsImpl[TToken, TPosition, TSyntaxError, TLabel[_], T]
      (grammar1: => Grammar[TToken, TPosition, TSyntaxError, TLabel, T]) {

      def -->[U](f: T => U): Grammar[TToken, TPosition, TSyntaxError, TLabel, U] = -+>(WithLocation.lift(f))

      def -+>[U](f: WithLocation[T, TPosition] => WithLocation[U, TPosition]): Grammar[TToken, TPosition, TSyntaxError, TLabel, U] =
        new MapGrammar[TToken, TPosition, TSyntaxError, TLabel, T, U](grammar1, _.map { (state, value) => (state, f(value)) })

      def |[U](grammar2: => Grammar[TToken, TPosition, TSyntaxError, TLabel, U])
        (implicit errorFactory: ErrorFactory[TToken, ?, TSyntaxError, TPosition])
        : Grammar[TToken, TPosition, TSyntaxError, TLabel, T | U] =
        new UnionGrammar[TToken, TPosition, TSyntaxError, TLabel, T | U](grammar1, grammar2)

      def ++[U, V](grammar2: => Grammar[TToken, TPosition, TSyntaxError, TLabel, U])
        (implicit combiner: GrammarConcatCombiner[T, U, V])
        : Grammar[TToken, TPosition, TSyntaxError, TLabel, V] =
        ConcatGrammar(grammar1, grammar2) { (a, b) =>
          WithLocation(combiner.combine(a.value, b.value), Location.merge(a.location, b.location))
        }

      def ++![U, V](grammar2: => Grammar[TToken, TPosition, TSyntaxError, TLabel, U])
        (implicit combiner: GrammarConcatCombiner[T, U, V])
        : Grammar[TToken, TPosition, TSyntaxError, TLabel, V] =
        ConcatGrammar(grammar1, StrictGrammar(grammar2)) { (a, b) =>
          WithLocation(combiner.combine(a.value, b.value), Location.merge(a.location, b.location))
        }

      def discard: Grammar[TToken, TPosition, TSyntaxError, TLabel, Unit] = --> { _ => () }

      def ?(implicit errorFactory: ErrorFactory[TToken, ?, TSyntaxError, TPosition])
        : Grammar[TToken, TPosition, TSyntaxError, TLabel, Option[T]] = -->(Some.apply) | EmptyStrGrammar(None)

      def +~ : Grammar[TToken, TPosition, TSyntaxError, TLabel, NonEmptyChunk[T]] = {
        lazy val grammar1Cached = grammar1
        grammar1Cached ++ (grammar1Cached*) --> { case (head, tail) => NonEmptyChunk(head, tail*) }
      }

      def * : Grammar[TToken, TPosition, TSyntaxError, TLabel, Chunk[T]] = new RepeatGrammar(grammar1)

      def observeLocation: Grammar[TToken, TPosition, TSyntaxError, TLabel, WithLocation[T, TPosition]] =
        -+> {
          case value @ WithLocation(_, location) => WithLocation(value, location)
        }

    }

    implicit def tuple2ConcatCombiner[A, B, C]: GrammarConcatCombiner[(A, B), C, (A, B, C)] = (t, c) => (t._1, t._2, c)

    implicit def tuple3ConcatCombiner[A, B, C, D]: GrammarConcatCombiner[(A, B, C), D, (A, B, C, D)] =
      (t, d) => (t._1, t._2, t._3, d)

    implicit def tuple4ConcatCombiner[A, B, C, D, E]: GrammarConcatCombiner[(A, B, C, D), E, (A, B, C, D, E)] =
      (t, e) => (t._1, t._2, t._3, t._4, e)

    implicit def tuple5ConcatCombiner[A, B, C, D, E, F]: GrammarConcatCombiner[(A, B, C, D, E), F, (A, B, C, D, E, F)] =
      (t, f) => (t._1, t._2, t._3, t._4, t._5, f)

    implicit def tuple6ConcatCombiner[A, B, C, D, E, F, G]
      : GrammarConcatCombiner[(A, B, C, D, E, F), G, (A, B, C, D, E, F, G)] =
      (t, g) => (t._1, t._2, t._3, t._4, t._5, t._6, g)

    implicit def tuple7ConcatCombiner[A, B, C, D, E, F, G, H]
      : GrammarConcatCombiner[(A, B, C, D, E, F, G), H, (A, B, C, D, E, F, G, H)] =
      (t, h) => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, h)

    implicit def tuple8ConcatCombiner[A, B, C, D, E, F, G, H, I]
      : GrammarConcatCombiner[(A, B, C, D, E, F, G, H), I, (A, B, C, D, E, F, G, H, I)] =
      (t, i) => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, i)

    implicit def tuple9ConcatCombiner[A, B, C, D, E, F, G, H, I, J]
      : GrammarConcatCombiner[(A, B, C, D, E, F, G, H, I), J, (A, B, C, D, E, F, G, H, I, J)] =
      (t, j) => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, j)

    implicit def tuple10ConcatCombiner[A, B, C, D, E, F, G, H, I, J, K]
      : GrammarConcatCombiner[(A, B, C, D, E, F, G, H, I, J), K, (A, B, C, D, E, F, G, H, I, J, K)] =
      (t, k) => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, k)

    implicit def tuple11ConcatCombiner[A, B, C, D, E, F, G, H, I, J, K, L]
      : GrammarConcatCombiner[(A, B, C, D, E, F, G, H, I, J, K), L, (A, B, C, D, E, F, G, H, I, J, K, L)] =
      (t, l) => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, l)

    implicit def tuple12ConcatCombiner[A, B, C, D, E, F, G, H, I, J, K, L, M]
      : GrammarConcatCombiner[(A, B, C, D, E, F, G, H, I, J, K, L), M, (A, B, C, D, E, F, G, H, I, J, K, L, M)] =
      (t, m) => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, m)

    implicit def tuple13ConcatCombiner[A, B, C, D, E, F, G, H, I, J, K, L, M, N]
      : GrammarConcatCombiner[(A, B, C, D, E, F, G, H, I, J, K, L, M), N, (A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
      (t, n) => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, n)

  }

  def token[TToken <: Matchable, TPosition, TSyntaxError, TLabel[_], TTokenCategory](category: TTokenCategory, tokenMatches: TToken => Boolean)
    (implicit errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError, TPosition])
    : Grammar[TToken, TPosition, TSyntaxError, TLabel, TToken] = matcher(category, (t: TToken) => Some(t).filter(tokenMatches))

  def eof[TToken, TPosition, TSyntaxError, TLabel[_], TTokenCategory, TResult](result: TResult)
    (implicit errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError, TPosition])
    : Grammar[TToken, TPosition, TSyntaxError, TLabel, TResult] = EndOfFileGrammar(result)

  def matcher[TToken <: Matchable, TPosition, TSyntaxError, TLabel[_], TTokenCategory, Result]
    (category: TTokenCategory, tokenMatcher: TToken => Option[Result])
    (implicit errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError, TPosition])
    : Grammar[TToken, TPosition, TSyntaxError, TLabel, Result] = matcher(category, TokenMatcher.Anything(tokenMatcher))

  def partialMatcher[TToken <: Matchable, TPosition, TSyntaxError, TLabel[_], TTokenCategory, Result](category: TTokenCategory)
    (f: PartialFunction[TToken, Result])(implicit errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError, TPosition])
    : Grammar[TToken, TPosition, TSyntaxError, TLabel, Result] = matcher(category, f.lift)

  def matcher[TToken <: Matchable, TPosition, TSyntaxError, TLabel[_], TTokenCategory, Result]
    (category: TTokenCategory, tokenMatcher: TokenMatcher[TToken, Result])
    (implicit errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError, TPosition])
    : Grammar[TToken, TPosition, TSyntaxError, TLabel, Result] = TokenGrammar(category, tokenMatcher)

  def reject[TToken, TPosition, TSyntaxError, TLabel[_], TTokenCategory, TResult](grammarErrors: NonEmptyChunk[TSyntaxError])
    : Grammar[TToken, TPosition, TSyntaxError, TLabel, TResult] = RejectGrammar(grammarErrors)

  def parseAll[TToken, TPosition, TSyntaxError, TLabel[_], T](factory: GrammarFactory[TToken, TPosition, TSyntaxError, TLabel])
    (label: TLabel[T])
    : ZChannel[Any, Nothing, Chunk[WithLocation[TToken, TPosition]], TPosition, TSyntaxError, Chunk[T], TPosition] =
    sealed trait ParseStep
    case object ParseStepReady extends ParseStep
    final case class ParseStepPartial(suspend: GrammarResultSuspend[TToken, TPosition, TSyntaxError, TLabel, T]) extends ParseStep

    val rule = factory(label)
    val defaultParseOptions: ParseOptions[TToken, TPosition, TSyntaxError, TLabel] = ParseOptions(Set.empty, None, factory)

    def runParseStepMulti(s: ParseStep, tokens: NonEmptyChunk[WithLocation[TToken, TPosition]], acc: Chunk[T])
      : IO[TSyntaxError, (ParseStep, Chunk[T])] =
      runParseStep(s, tokens) match {
        case GrammarResultSuccess(remaining, WithLocation(value, _)) =>
          NonEmptyChunk.fromChunk(remaining) match {
            case Some(remaining) => runParseStepMulti(ParseStepReady, remaining, acc :+ value)
            case None => ZIO.succeed((ParseStepReady, acc :+ value))
          }

        case GrammarResultFailure(failure) => ZIO.failCause(ZIOErrorUtil.multiCauseChunk(failure))
        case GrammarResultError(error) => ZIO.failCause(ZIOErrorUtil.multiCauseChunk(error))
        case suspend: GrammarResultSuspend[TToken, TPosition, TSyntaxError, TLabel, T] =>
          ZIO.succeed((ParseStepPartial(suspend), acc))
      }

    def runParseStep(s: ParseStep, tokens: NonEmptyChunk[WithLocation[TToken, TPosition]])
      : GrammarResult[TToken, TPosition, TSyntaxError, TLabel, T] =
      s match {
        case _: ParseStepReady.type => rule.parseTokens(tokens, defaultParseOptions)
        case ParseStepPartial(suspend) => suspend.continue(tokens)
      }

    def consumeTokens(state: ParseStep)(tokens: Chunk[WithLocation[TToken, TPosition]])
      : ZChannel[Any, Nothing, Chunk[WithLocation[TToken, TPosition]], TPosition, TSyntaxError, Chunk[T], TPosition] =
      NonEmptyChunk.fromChunk(tokens) match {
        case Some(neTokens) =>
          ZChannel.fromZIO(runParseStepMulti(state, neTokens, Chunk.empty))
            .flatMap { case (state, values) =>
              ZChannel.write(values).flatMap { _ =>
                parse(state)
              }
            }

        case None => parse(state)
      }

    def finish(state: ParseStep)(end: TPosition)
      : ZChannel[Any, Any, Any, Any, TSyntaxError, Chunk[T], TPosition] =
      state match {
        case _: ParseStepReady.type => ZChannel.succeed(end)
        case ParseStepPartial(suspend) =>
          def parseEnd(result: GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, T], acc: Chunk[T])
            : ZChannel[Any, Any, Any, Any, TSyntaxError, Chunk[T], TPosition] =
            result match {
              case GrammarResultSuccess(remaining, WithLocation(value, _)) =>
                NonEmptyChunk.fromChunk(remaining) match {
                  case Some(tokens) =>
                    parseEnd(rule.parseTokens(tokens, defaultParseOptions).completeResult(end), acc :+ value)
                  case None => ZChannel.write(acc :+ value).as(end)
                }

              case GrammarResultFailure(failure) => ZChannel.failCause(ZIOErrorUtil.multiCauseChunk(failure))
              case GrammarResultError(error) => ZChannel.failCause(ZIOErrorUtil.multiCauseChunk(error))
            }

          parseEnd(suspend.completeResult(end), Chunk.empty)
      }

    def parse(state: ParseStep)
      : ZChannel[Any, Nothing, Chunk[WithLocation[TToken, TPosition]], TPosition, TSyntaxError, Chunk[T], TPosition] =
      ZChannel.readWithCause(
        in = consumeTokens(state),
        halt = ZChannel.failCause(_),
        done = finish(state),
      )

    parse(ParseStepReady)

  end parseAll

  trait ErrorFactory[-TToken, -TTokenCategory, TSyntaxError, TPosition] {
    def createError(error: GrammarError[TToken, TTokenCategory, TPosition]): TSyntaxError
    given errorEndLocationOrder: Ordering[TSyntaxError]
  }

  type TokenMatcherFunc[TToken, T, TPosition] = WithLocation[TToken, TPosition] => Option[WithLocation[T, TPosition]]

  private final case class RejectGrammar[TToken, TPosition, TSyntaxError, TLabel[_], T](grammarErrors: NonEmptyChunk[TSyntaxError])
      extends Grammar[TToken, TPosition, TSyntaxError, TLabel, T] {

    override def parseTokens(tokens: NonEmptyChunk[WithLocation[TToken, TPosition]], options: TParseOptions)
      : GrammarResult[TToken, TPosition, TSyntaxError, TLabel, T] = GrammarResultFailure(grammarErrors)

    override def parseEnd(pos: TPosition, options: TParseOptions)
      : GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, T] = GrammarResultFailure(grammarErrors)

  }

  private final case class EmptyStrGrammar[TToken, TPosition, TSyntaxError, TLabel[_], T](result: T)
      extends Grammar[TToken, TPosition, TSyntaxError, TLabel, T] {

    override def parseTokens(tokens: NonEmptyChunk[WithLocation[TToken, TPosition]], options: TParseOptions)
      : GrammarResult[TToken, TPosition, TSyntaxError, TLabel, T] =
      GrammarResultSuccess(
        tokens,
        WithLocation(result, Location(options.factory.fileName, tokens.head.location.start, tokens.head.location.start)),
      )

    override def parseEnd(pos: TPosition, options: TParseOptions)
      : GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, T] =
      GrammarResultSuccess(Chunk(), WithLocation(result, Location(options.factory.fileName, pos, pos)))

  }

  private final case class EndOfFileGrammar[TToken, TPosition, TSyntaxError, TLabel[_], TTokenCategory, T]
    (
      result: T
    )
    (
      implicit errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError, TPosition]
    ) extends Grammar[TToken, TPosition, TSyntaxError, TLabel, T] {

    override def parseTokens(tokens: NonEmptyChunk[WithLocation[TToken, TPosition]], options: TParseOptions)
      : GrammarResult[TToken, TPosition, TSyntaxError, TLabel, T] =
      GrammarResultFailure(NonEmptyChunk(errorFactory.createError(GrammarError.ExpectedEndOfFile(tokens.head))))

    override def parseEnd(pos: TPosition, options: TParseOptions)
      : GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, T] =
      GrammarResultSuccess(Chunk(), WithLocation(result, Location(options.factory.fileName, pos, pos)))

  }

  private final case class TokenGrammar[TToken <: Matchable, TPosition, TSyntaxError, TLabel[_], TTokenCategory, T]
    (
      category: TTokenCategory,
      tokenMatcher: TokenMatcher[TToken, T],
    )
    (implicit
      errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError, TPosition]
    ) extends Grammar[TToken, TPosition, TSyntaxError, TLabel, T] {

    override def parseTokens(tokens: NonEmptyChunk[WithLocation[TToken, TPosition]], options: TParseOptions)
      : GrammarResult[TToken, TPosition, TSyntaxError, TLabel, T] = {

      object MatchingToken {
        def unapply(arg: TToken): Option[T] = tokenMatcher.matchToken(arg)
      }

      tokens.head match {
        case WithLocation(MatchingToken(value), loc) =>
          GrammarResultSuccess(
            tokens.tail,
            WithLocation(value, loc),
          )
        case token =>
          GrammarResultFailure(NonEmptyChunk(errorFactory.createError(GrammarError.UnexpectedToken(category, token))))
      }
    }

    override def parseEnd(pos: TPosition, options: TParseOptions)
      : GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, T] =
      GrammarResultFailure(NonEmptyChunk(errorFactory.createError(GrammarError.UnexpectedEndOfFile(category, options.factory.fileName, pos))))

  }

  private final class ConcatGrammar[TToken, TPosition, TSyntaxError, TLabel[_], A, B, T]
    (
      grammarAUncached: => Grammar[TToken, TPosition, TSyntaxError, TLabel, A],
      grammarBUncached: => Grammar[TToken, TPosition, TSyntaxError, TLabel, B],
      combine: (WithLocation[A, TPosition], WithLocation[B, TPosition]) => WithLocation[T, TPosition],
    ) extends Grammar[TToken, TPosition, TSyntaxError, TLabel, T] {

    private lazy val grammarA = grammarAUncached
    private lazy val grammarB = grammarBUncached

    private def nextHandler(valueA: WithLocation[A, TPosition], options: TParseOptions)
      : GrammarResultSuspend[TToken, TPosition, TSyntaxError, TLabel, T] =
      new GrammarResultSuspend[TToken, TPosition, TSyntaxError, TLabel, T] {

        override def continue(tokens: NonEmptyChunk[WithLocation[TToken, TPosition]])
          : GrammarResult[TToken, TPosition, TSyntaxError, TLabel, T] =
          grammarB.parseTokens(tokens, options.notLeftRec).map {
            case (tokens3, valueB) =>
              (tokens3, combine(valueA, valueB))
          }

        override def completeResult(pos: TPosition): GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, T] =
          grammarB.parseEnd(pos, options.notLeftRec).map {
            case (state3, valueB) =>
              (state3, combine(valueA, valueB))
          }

      }

    override def parseTokens(tokens: NonEmptyChunk[WithLocation[TToken, TPosition]], options: TParseOptions)
      : GrammarResult[TToken, TPosition, TSyntaxError, TLabel, T] =
      grammarA.parseTokens(tokens, options).flatMap {
        case (ChunkUnCons(nextTokens), valueA) =>
          nextHandler(valueA, options).continue(nextTokens)

        case (_, valueA) =>
          nextHandler(valueA, options)
      }

    override def parseEnd(pos: TPosition, options: TParseOptions)
      : GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, T] =
      grammarA.parseEnd(pos, options).flatMap[T] { (remaining: Chunk[WithLocation[TToken, TPosition]], valueA: WithLocation[A, TPosition]) =>
        remaining match {
          case ChunkUnCons(nextTokens) =>
            nextHandler(valueA, options).continue(nextTokens).completeResult(pos)

          case _ =>
            nextHandler(valueA, options).completeResult(pos)
        }
      }

  }

  private object ConcatGrammar {

    def apply[TToken, TPosition, TSyntaxError, TLabel[_], A, B, T]
      (
        grammarA: => Grammar[TToken, TPosition, TSyntaxError, TLabel, A],
        grammarB: => Grammar[TToken, TPosition, TSyntaxError, TLabel, B],
      )
      (
        combine: (WithLocation[A, TPosition], WithLocation[B, TPosition]) => WithLocation[T, TPosition]
      )
      : ConcatGrammar[TToken, TPosition, TSyntaxError, TLabel, A, B, T] = new ConcatGrammar(grammarA, grammarB, combine)

  }

  private final class StrictGrammar[TToken, TPosition, TSyntaxError, TLabel[_], T]
    (
      innerUncached: => Grammar[TToken, TPosition, TSyntaxError, TLabel, T]
    ) extends Grammar[TToken, TPosition, TSyntaxError, TLabel, T] {

    private lazy val inner = innerUncached

    override def parseTokens(tokens: NonEmptyChunk[WithLocation[TToken, TPosition]], options: TParseOptions)
      : GrammarResult[TToken, TPosition, TSyntaxError, TLabel, T] = inner.parseTokens(tokens, options).treatFailureAsError

    override def parseEnd(pos: TPosition, options: TParseOptions)
      : GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, T] = inner.parseEnd(pos, options).treatFailureAsError

  }

  private object StrictGrammar {

    def apply[TToken, TPosition, TSyntaxError, TLabel[_], T]
      (
        inner: => Grammar[TToken, TPosition, TSyntaxError, TLabel, T]
      )
      : StrictGrammar[TToken, TPosition, TSyntaxError, TLabel, T] = new StrictGrammar(inner)

  }

  private final class ParseStateGrammar[TToken, TPosition, TSyntaxError, TLabel[_], T]
    (
      innerUncached: => Grammar[TToken, TPosition, TSyntaxError, TLabel, T],
      prevTokens: Chunk[WithLocation[TToken, TPosition]],
      prevOptions: ParseOptions[TToken, TPosition, TSyntaxError, TLabel],
    ) extends Grammar[TToken, TPosition, TSyntaxError, TLabel, T] {

    private lazy val inner = innerUncached

    override def parseTokens(tokens: NonEmptyChunk[WithLocation[TToken, TPosition]], options: TParseOptions)
      : GrammarResult[TToken, TPosition, TSyntaxError, TLabel, T] = inner.parseTokens(tokens.prepend(prevTokens), prevOptions)

    override def parseEnd(pos: TPosition, options: TParseOptions)
      : GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, T] =
      prevTokens match {
        case ChunkUnCons(tokens) =>
          inner.parseTokens(tokens, prevOptions).completeResult(pos)
        case _ => inner.parseEnd(pos, prevOptions)
      }

  }

  private final class UnionGrammar[TToken, TPosition, TSyntaxError, TLabel[_], T]
    (
      grammarAUncached: => Grammar[TToken, TPosition, TSyntaxError, TLabel, T],
      grammarBUncached: => Grammar[TToken, TPosition, TSyntaxError, TLabel, T],
    )(implicit errorFactory: ErrorFactory[TToken, ?, TSyntaxError, TPosition])
      extends Grammar[TToken, TPosition, TSyntaxError, TLabel, T] {
    import errorFactory.errorEndLocationOrder

    private lazy val grammarA = grammarAUncached
    private lazy val grammarB = grammarBUncached

    override def parseTokens(tokens: NonEmptyChunk[WithLocation[TToken, TPosition]], options: TParseOptions)
      : GrammarResult[TToken, TPosition, TSyntaxError, TLabel, T] =
      grammarA.parseTokens(tokens, options).recoverFailure {
        case (laterTokens, GrammarResultFailure(errorListA)) =>
          grammarB.parseTokens(tokens ++ laterTokens, options).transformComplete {
            case result @ GrammarResultSuccess(_, _) => result
            case result @ GrammarResultError(_) => result
            case GrammarResultFailure(errorListB) =>
              def findLastErrorPos(errorList: TErrorList): TSyntaxError = errorList.maximum

              GrammarResultFailure(
                errorFactory.errorEndLocationOrder.compare(
                  findLastErrorPos(errorListA),
                  findLastErrorPos(errorListB),
                ) match {
                  case o if o > 0 => errorListA
                  case o if o < 0 => errorListB
                  case _ => errorListA ++ errorListB
                }
              )
          }
      }

    override def parseEnd(pos: TPosition, options: TParseOptions)
      : GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, T] =
      grammarA.parseEnd(pos, options) match {
        case result @ GrammarResultSuccess(_, _) => result
        case result @ GrammarResultError(_) => result
        case GrammarResultFailure(errorListA) =>
          grammarB.parseEnd(pos, options) match {
            case result @ GrammarResultSuccess(_, _) => result
            case result @ GrammarResultError(_) => result
            case GrammarResultFailure(errorListB) =>
              def findLastErrorPos(errorList: TErrorList): TSyntaxError = errorList.maximum

              GrammarResultFailure(
                errorFactory.errorEndLocationOrder.compare(
                  findLastErrorPos(errorListA),
                  findLastErrorPos(errorListB),
                ) match {
                  case o if o > 0 => errorListA
                  case o if o < 0 => errorListB
                  case _ => errorListA ++ errorListB
                }
              )
          }
      }

  }

  object UnionGrammar {

    def apply[TToken, TPosition, TSyntaxError, TLabel[_], T]
      (grammarA: => Grammar[TToken, TPosition, TSyntaxError, TLabel, T], grammarB: => Grammar[TToken, TPosition, TSyntaxError, TLabel, T])
      (implicit errorFactory: ErrorFactory[TToken, ?, TSyntaxError, TPosition])
      : Grammar[TToken, TPosition, TSyntaxError, TLabel, T] = new UnionGrammar[TToken, TPosition, TSyntaxError, TLabel, T](grammarA, grammarB)

    def fromList[TToken, TPosition, TSyntaxError, TLabel[_], T]
      (grammars: NonEmptyChunk[Lazy[Grammar[TToken, TPosition, TSyntaxError, TLabel, T]]])
      (implicit errorFactory: ErrorFactory[TToken, ?, TSyntaxError, TPosition])
      : Grammar[TToken, TPosition, TSyntaxError, TLabel, T] =
      grammars.tail match {
        case ChunkUnCons(grammarsTail) =>
          grammarsTail.tail.foldLeft(apply(grammars.head.value, grammarsTail.head.value)) { (a, b) =>
            apply(a, b.value)
          }

        case _ =>
          grammars.head.value
      }

  }

  private final class RepeatGrammar[TToken, TPosition, TSyntaxError, TLabel[_], T]
    (innerUncached: => Grammar[TToken, TPosition, TSyntaxError, TLabel, T])
      extends Grammar[TToken, TPosition, TSyntaxError, TLabel, Chunk[T]] {

    private lazy val inner = innerUncached

    override def parseTokens(tokens: NonEmptyChunk[WithLocation[TToken, TPosition]], options: TParseOptions)
      : GrammarResult[TToken, TPosition, TSyntaxError, TLabel, Chunk[T]] = parseInner(tokens, options, Chunk.empty)

    private def itemsLocation(fileName: Option[String], pos: TPosition, items: Chunk[WithLocation[T, TPosition]]): Location[TPosition] =
      items match {
        case WithLocation(_, loc1) +: _ :+ WithLocation(_, loc2) =>
          Location.merge(loc1, loc2)

        case Chunk(WithLocation(_, loc)) =>
          loc

        case _ => Location(fileName, pos, pos)
      }

    private def finalItems(items: Chunk[WithLocation[T, TPosition]], fileName: Option[String], pos: TPosition): WithLocation[Chunk[T], TPosition] =
      WithLocation(items.map(removeSource), itemsLocation(fileName, pos, items))

    private def parseInner
      (tokens: NonEmptyChunk[WithLocation[TToken, TPosition]], options: TParseOptions, items: Chunk[WithLocation[T, TPosition]])
      : GrammarResult[TToken, TPosition, TSyntaxError, TLabel, Chunk[T]] =
      inner.parseTokens(tokens, options).flatMap {
        case (ChunkUnCons(remaining), item) =>
          parseInner(remaining, options.notLeftRec, items :+ item)

        case (_, item) =>
          new GrammarResultSuspend[TToken, TPosition, TSyntaxError, TLabel, Chunk[T]] {

            override def continue(tokens: NonEmptyChunk[WithLocation[TToken, TPosition]])
              : GrammarResult[TToken, TPosition, TSyntaxError, TLabel, Chunk[T]] =
              parseInner(tokens, options.notLeftRec, items :+ item)

            override def completeResult(pos: TPosition)
              : GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, Chunk[T]] =
              GrammarResultSuccess(Chunk(), finalItems(items :+ item, options.factory.fileName, pos))

          }
      }
        .recoverFailure { (laterTokens, _) =>
          GrammarResultSuccess(tokens ++ laterTokens, finalItems(items, options.factory.fileName, tokens.head.location.start))
        }

    override def parseEnd(pos: TPosition, options: TParseOptions)
      : GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, Chunk[T]] =
      GrammarResultSuccess(Chunk(), WithLocation(Chunk.empty, Location(options.factory.fileName, pos, pos)))

    private def removeSource[A](ws: WithLocation[A, TPosition]): A = ws.value
  }

  private final class MapGrammar[TToken, TPosition, TSyntaxError, TLabel[_], T, U]
    (
      innerUncached: => Grammar[TToken, TPosition, TSyntaxError, TLabel, T],
      f: GrammarResult[TToken, TPosition, TSyntaxError, TLabel, T] => GrammarResult[TToken, TPosition, TSyntaxError, TLabel, U],
    ) extends Grammar[TToken, TPosition, TSyntaxError, TLabel, U] {

    private lazy val inner = innerUncached

    override def parseTokens(tokens: NonEmptyChunk[WithLocation[TToken, TPosition]], options: TParseOptions)
      : GrammarResult[TToken, TPosition, TSyntaxError, TLabel, U] = f(inner.parseTokens(tokens, options))

    override def parseEnd(pos: TPosition, options: TParseOptions)
      : GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, U] = f(inner.parseEnd(pos, options)).completeResult(pos)

  }

  private final class LabelRefGrammar[TToken, TPosition, TSyntaxError, TLabel[_], T](label: TLabel[T])
      extends Grammar[TToken, TPosition, TSyntaxError, TLabel, T] {

    override def parseTokens(tokens: NonEmptyChunk[WithLocation[TToken, TPosition]], options: TParseOptions)
      : GrammarResult[TToken, TPosition, TSyntaxError, TLabel, T] = options.factory(label).parseTokens(tokens, options)

    override def parseEnd(pos: TPosition, options: TParseOptions)
      : GrammarResultComplete[TToken, TPosition, TSyntaxError, TLabel, T] = options.factory(label).parseEnd(pos, options)

  }

  @SuppressWarnings(Array("scalafix:MissingFinal.trait"))
  trait EmbeddedGrammar[TSyntaxError, TPosition, TTokenA, TLabelA[_], TTokenB, TLabelB[_], T]
      extends Grammar[TTokenA, TPosition, TSyntaxError, TLabelA, T] {

    protected val outerGrammar: Grammar[TTokenA, TPosition, TSyntaxError, TLabelA, TTokenB]
    protected val innerGrammar: Grammar[TTokenB, TPosition, TSyntaxError, TLabelB, T]
    protected val innerFactory: GrammarFactory[TTokenB, TPosition, TSyntaxError, TLabelB]
    def stopToken(token: TTokenB): Boolean
    def unexpectedEndOfFileError(pos: TPosition): TSyntaxError
    def unexpectedToken(token: WithLocation[TTokenB, TPosition]): TSyntaxError

    final override def parseTokens(tokens: NonEmptyChunk[WithLocation[TTokenA, TPosition]], options: TParseOptions)
      : GrammarResult[TTokenA, TPosition, TSyntaxError, TLabelA, T] = {

      def handleResult(result: GrammarResult[TTokenA, TPosition, TSyntaxError, TLabelA, TTokenB], acc: Chunk[WithLocation[TTokenB, TPosition]])
        : GrammarResult[TTokenA, TPosition, TSyntaxError, TLabelA, T] =
        result match {
          case result: GrammarResultError[TTokenA, TPosition, TSyntaxError, TLabelA] => result
          case result: GrammarResultFailure[TTokenA, TPosition, TSyntaxError, TLabelA] => result
          case result: GrammarResultSuspend[TTokenA, TPosition, TSyntaxError, TLabelA, TTokenB] =>
            new GrammarResultSuspend[TTokenA, TPosition, TSyntaxError, TLabelA, T] {
              override def continue(tokens: NonEmptyChunk[WithLocation[TTokenA, TPosition]])
                : GrammarResult[TTokenA, TPosition, TSyntaxError, TLabelA, T] = handleResult(result.continue(tokens), acc)

              override def completeResult(pos: TPosition): GrammarResultComplete[TTokenA, TPosition, TSyntaxError, TLabelA, T] =
                handleResult(result.completeResult(pos), acc).completeResult(pos)
            }

          case GrammarResultSuccess(extra, tokenB) if stopToken(tokenB.value) =>
            def handleFinalResult(result: GrammarResultComplete[TTokenB, TPosition, TSyntaxError, TLabelB, T])
              : GrammarResult[TTokenA, TPosition, TSyntaxError, TLabelA, T] =
              result match {
                case GrammarResultFailure(failure) => GrammarResultFailure(failure)
                case GrammarResultError(error) => GrammarResultError(error)

                case GrammarResultSuccess(ChunkUnCons(tokens), _) =>
                  GrammarResultFailure(NonEmptyChunk(unexpectedToken(tokens.head)))

                case GrammarResultSuccess(_, value) =>
                  GrammarResultSuccess(extra, value)
              }

            NonEmptyChunk.fromChunk(acc) match {
              case Some(acc) =>
                innerGrammar.parseTokens(acc, ParseOptions(Set.empty, None, innerFactory)) match {
                  case GrammarResultError(errors) => GrammarResultError(errors)
                  case GrammarResultFailure(failure) => GrammarResultFailure(failure)
                  case innerResult: GrammarResultSuspend[TTokenB, TPosition, TSyntaxError, TLabelB, T] =>
                    handleFinalResult(innerResult.completeResult(tokenB.location.start))

                  case GrammarResultSuccess(ChunkUnCons(tokens), _) =>
                    GrammarResultFailure(NonEmptyChunk(unexpectedToken(tokens.head)))

                  case GrammarResultSuccess(_, value) =>
                    GrammarResultSuccess(extra, value)
                }

              case None =>
                handleFinalResult(innerGrammar.parseEnd(
                  tokenB.location.start,
                  ParseOptions(Set.empty, None, innerFactory),
                ))
            }

          case GrammarResultSuccess(ChunkUnCons(tokens), tokenB) =>
            handleResult(outerGrammar.parseTokens(tokens, options), acc :+ tokenB)

          case GrammarResultSuccess(_, tokenB) =>
            new GrammarResultSuspend[TTokenA, TPosition, TSyntaxError, TLabelA, T] {
              override def continue(tokens: NonEmptyChunk[WithLocation[TTokenA, TPosition]])
                : GrammarResult[TTokenA, TPosition, TSyntaxError, TLabelA, T] =
                handleResult(outerGrammar.parseTokens(tokens, options), acc :+ tokenB)

              override def completeResult(pos: TPosition): GrammarResultComplete[TTokenA, TPosition, TSyntaxError, TLabelA, T] =
                GrammarResultFailure(NonEmptyChunk(unexpectedEndOfFileError(pos)))
            }
        }

      handleResult(outerGrammar.parseTokens(tokens, options), Chunk.empty)
    }

    final override def parseEnd(pos: TPosition, options: TParseOptions)
      : GrammarResultComplete[TTokenA, TPosition, TSyntaxError, TLabelA, T] =
      GrammarResultFailure(NonEmptyChunk(unexpectedEndOfFileError(pos)))

  }

}
