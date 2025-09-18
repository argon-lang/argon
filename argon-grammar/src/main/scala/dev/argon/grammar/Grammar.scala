package dev.argon.grammar

import dev.argon.grammar.Grammar.*
import scala.collection.immutable.{Stream as _, *}
import scala.util.NotGiven
import dev.argon.util.{*, given}
import dev.argon.util.async.*
import zio.*
import zio.stream.*
import zio.interop.catz.core.given
import cats.*
import cats.implicits.given

sealed trait Grammar[TToken, TTokenCategory, TPosition, TLabel[_], +T] {
  type TSyntaxError = GrammarError[TToken, TTokenCategory, TPosition]

  type TErrorList = NonEmptyChunk[TSyntaxError]
  type TParseOptions = ParseOptions[TToken, TTokenCategory, TPosition, TLabel]

  def parseTokens(tokens: NonEmptyChunk[WithLocation[TToken, TPosition]], options: TParseOptions)
    : GrammarResult[TToken, TTokenCategory, TPosition, TLabel, T]

  def parseEnd(pos: TPosition, options: TParseOptions): GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, T]

}

object Grammar {

  abstract class GrammarFactory[TToken, TTokenCategory, TPosition, TLabel[_]] {
    type TSyntaxError = GrammarError[TToken, TTokenCategory, TPosition]
    type TGrammar[+T] = Grammar[TToken, TTokenCategory, TPosition, TLabel, T]

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

    def rule[T](label: TLabel[T]): Grammar[TToken, TTokenCategory, TPosition, TLabel, T] =
      new LabelRefGrammar[TToken, TTokenCategory, TPosition, TLabel, T](label)

  }

  sealed trait GrammarResult[TToken, TTokenCategory, TPosition, TLabel[_], +T] {
    type TSyntaxError = GrammarError[TToken, TTokenCategory, TPosition]

    def map[U](f: GrammarResultSuccess[TToken, TTokenCategory, TPosition, TLabel, T] => GrammarResultSuccess[TToken, TTokenCategory, TPosition, TLabel, U])
      : GrammarResult[TToken, TTokenCategory, TPosition, TLabel, U]

    def flatMap[U](f: GrammarResultSuccess[TToken, TTokenCategory, TPosition, TLabel, T] => GrammarResult[TToken, TTokenCategory, TPosition, TLabel, U])
      : GrammarResult[TToken, TTokenCategory, TPosition, TLabel, U]

    def treatFailureAsError: GrammarResult[TToken, TTokenCategory, TPosition, TLabel, T]

    def treatFailureAsErrorWhen(b: Boolean): GrammarResult[TToken, TTokenCategory, TPosition, TLabel, T] =
      if b then treatFailureAsError
      else this

    def recoverFailure[U >: T]
      (f: (Chunk[WithLocation[TToken, TPosition]], GrammarResultFailure[TToken, TTokenCategory, TPosition, TLabel]) => GrammarResult[
        TToken,
        TTokenCategory,
        TPosition,
        TLabel,
        U,
      ])
      : GrammarResult[TToken, TTokenCategory, TPosition, TLabel, U]

    def transformComplete[U]
      (f: GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, T] => GrammarResult[TToken, TTokenCategory, TPosition, TLabel, U])
      : GrammarResult[TToken, TTokenCategory, TPosition, TLabel, U]

    def completeResult(pos: TPosition): GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, T]
  }

  sealed trait GrammarResultNonSuccess[TToken, TTokenCategory, TPosition, TLabel[_], +T]
      extends GrammarResult[TToken, TTokenCategory, TPosition, TLabel, T]

  sealed trait GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel[_], +T]
      extends GrammarResult[TToken, TTokenCategory, TPosition, TLabel, T] {
    def toEither: Either[NonEmptyChunk[TSyntaxError], (Chunk[WithLocation[TToken, TPosition]], WithLocation[T, TPosition])]

    def flatMap[U]
      (f: GrammarResultSuccess[TToken, TTokenCategory, TPosition, TLabel, T] => GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, U])
      : GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, U]

    override def map[U](f: GrammarResultSuccess[TToken, TTokenCategory, TPosition, TLabel, T] => GrammarResultSuccess[TToken, TTokenCategory, TPosition, TLabel, U])
      : GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, U]

    override def flatMap[U]
      (f: GrammarResultSuccess[TToken, TTokenCategory, TPosition, TLabel, T] => GrammarResult[TToken, TTokenCategory, TPosition, TLabel, U])
      : GrammarResult[TToken, TTokenCategory, TPosition, TLabel, U]

    override def treatFailureAsError: GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, T]

    override def treatFailureAsErrorWhen(b: Boolean): GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, T] =
      if b then treatFailureAsError
      else this

    override def transformComplete[U]
      (f: GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, T] => GrammarResult[TToken, TTokenCategory, TPosition, TLabel, U])
      : GrammarResult[TToken, TTokenCategory, TPosition, TLabel, U] = f(this)

    override def completeResult(pos: TPosition): GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, T] = this
  }

  final case class GrammarResultSuccess[TToken, TTokenCategory, TPosition, TLabel[_], +T]
    (tokens: Chunk[WithLocation[TToken, TPosition]], value: WithLocation[T, TPosition])
      extends GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, T] {

    override def map[U](f: GrammarResultSuccess[TToken, TTokenCategory, TPosition, TLabel, T] => GrammarResultSuccess[TToken, TTokenCategory, TPosition, TLabel, U])
      : GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, U] =
      f(this)

    override def flatMap[U]
      (f: GrammarResultSuccess[TToken, TTokenCategory, TPosition, TLabel, T] => GrammarResult[TToken, TTokenCategory, TPosition, TLabel, U])
      : GrammarResult[TToken, TTokenCategory, TPosition, TLabel, U] = f(this)

    override def flatMap[U]
      (f: GrammarResultSuccess[TToken, TTokenCategory, TPosition, TLabel, T] => GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, U])
      : GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, U] = f(this)

    override def treatFailureAsError: GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, T] =
      this

    override def recoverFailure[U >: T]
      (f: (Chunk[WithLocation[TToken, TPosition]], GrammarResultFailure[TToken, TTokenCategory, TPosition, TLabel]) => GrammarResult[
        TToken,
        TTokenCategory,
        TPosition,
        TLabel,
        U,
      ])
      : GrammarResult[TToken, TTokenCategory, TPosition, TLabel, U] = this

    override def toEither: Either[NonEmptyChunk[TSyntaxError], (Chunk[WithLocation[TToken, TPosition]], WithLocation[T, TPosition])] =
      Right((tokens, value))

  }

  final case class GrammarResultFailure[TToken, TTokenCategory, TPosition, TLabel[_]](failure: NonEmptyChunk[GrammarError[TToken, TTokenCategory, TPosition]])
      extends GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, Nothing]
      with GrammarResultNonSuccess[TToken, TTokenCategory, TPosition, TLabel, Nothing] {

    override def map[U]
      (f: GrammarResultSuccess[TToken, TTokenCategory, TPosition, TLabel, Nothing] => GrammarResultSuccess[TToken, TTokenCategory, TPosition, TLabel, U])
      : GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, U] = this

    override def flatMap[U]
      (f: GrammarResultSuccess[TToken, TTokenCategory, TPosition, TLabel, Nothing] => GrammarResult[TToken, TTokenCategory, TPosition, TLabel, U])
      : GrammarResult[TToken, TTokenCategory, TPosition, TLabel, U] = this

    override def flatMap[U]
      (f: GrammarResultSuccess[TToken, TTokenCategory, TPosition, TLabel, Nothing] => GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, U])
      : GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, U] = this

    override def treatFailureAsError: GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, Nothing] =
      GrammarResultError(failure)

    override def recoverFailure[U >: Nothing]
      (f: (Chunk[WithLocation[TToken, TPosition]], GrammarResultFailure[TToken, TTokenCategory, TPosition, TLabel]) => GrammarResult[
        TToken,
        TTokenCategory,
        TPosition,
        TLabel,
        U,
      ])
      : GrammarResult[TToken, TTokenCategory, TPosition, TLabel, U] = f(Chunk.empty, this)

    override def toEither: Either[NonEmptyChunk[TSyntaxError], Nothing] = Left(failure)
  }

  final case class GrammarResultError[TToken, TTokenCategory, TPosition, TLabel[_]](error: NonEmptyChunk[GrammarError[TToken, TTokenCategory, TPosition]])
      extends GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, Nothing]
      with GrammarResultNonSuccess[TToken, TTokenCategory, TPosition, TLabel, Nothing] {

    override def map[U]
      (f: GrammarResultSuccess[TToken, TTokenCategory, TPosition, TLabel, Nothing] => GrammarResultSuccess[TToken, TTokenCategory, TPosition, TLabel, U])
      : GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, U] = this

    override def flatMap[U]
      (f: GrammarResultSuccess[TToken, TTokenCategory, TPosition, TLabel, Nothing] => GrammarResult[TToken, TTokenCategory, TPosition, TLabel, U])
      : GrammarResult[TToken, TTokenCategory, TPosition, TLabel, U] = this

    override def flatMap[U]
      (f: GrammarResultSuccess[TToken, TTokenCategory, TPosition, TLabel, Nothing] => GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, U])
      : GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, U] = this

    override def treatFailureAsError: GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, Nothing] = this

    override def recoverFailure[U >: Nothing]
      (f: (Chunk[WithLocation[TToken, TPosition]], GrammarResultFailure[TToken, TTokenCategory, TPosition, TLabel]) => GrammarResult[
        TToken,
        TTokenCategory,
        TPosition,
        TLabel,
        U,
      ])
      : GrammarResult[TToken, TTokenCategory, TPosition, TLabel, U] = this

    override def toEither: Either[NonEmptyChunk[TSyntaxError], Nothing] = Left(error)
  }

  sealed trait GrammarResultSuspend[TToken, TTokenCategory, TPosition, TLabel[_], +T]
      extends GrammarResultNonSuccess[TToken, TTokenCategory, TPosition, TLabel, T] {

    val hasCut: Boolean

    def continue(tokens: NonEmptyChunk[WithLocation[TToken, TPosition]]): GrammarResult[TToken, TTokenCategory, TPosition, TLabel, T]

    override def map[U](f: GrammarResultSuccess[TToken, TTokenCategory, TPosition, TLabel, T] => GrammarResultSuccess[TToken, TTokenCategory, TPosition, TLabel, U])
      : GrammarResult[TToken, TTokenCategory, TPosition, TLabel, U] = transformComplete(_.map(f))

    override def flatMap[U]
      (f: GrammarResultSuccess[TToken, TTokenCategory, TPosition, TLabel, T] => GrammarResult[TToken, TTokenCategory, TPosition, TLabel, U])
      : GrammarResult[TToken, TTokenCategory, TPosition, TLabel, U] = transformComplete(_.flatMap(f))

    override def treatFailureAsError: GrammarResult[TToken, TTokenCategory, TPosition, TLabel, T] =
      transformCompleteWithCut(true)(_.treatFailureAsError)

    override def recoverFailure[U >: T]
      (f: (Chunk[WithLocation[TToken, TPosition]], GrammarResultFailure[TToken, TTokenCategory, TPosition, TLabel]) => GrammarResult[
        TToken,
        TTokenCategory,
        TPosition,
        TLabel,
        U,
      ])
      : GrammarResult[TToken, TTokenCategory, TPosition, TLabel, U] =
      if hasCut then
        this
      else
        new GrammarResultSuspend[TToken, TTokenCategory, TPosition, TLabel, U] {
          override val hasCut: Boolean = false

          override def continue(tokens: NonEmptyChunk[WithLocation[TToken, TPosition]])
            : GrammarResult[TToken, TTokenCategory, TPosition, TLabel, U] =
            GrammarResultSuspend.this.continue(tokens).recoverFailure { (tokens2, failure) =>
              f(tokens ++ tokens2, failure)
            }

          override def completeResult(pos: TPosition): GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, U] =
            GrammarResultSuspend.this.completeResult(pos).recoverFailure(f).completeResult(pos)

        }

    override def transformComplete[U]
      (f: GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, T] => GrammarResult[TToken, TTokenCategory, TPosition, TLabel, U])
      : GrammarResult[TToken, TTokenCategory, TPosition, TLabel, U] =
      transformCompleteWithCut(hasCut)(f)

    private def transformCompleteWithCut[U]
    (hasCutValue: Boolean)
    (f: GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, T] => GrammarResult[TToken, TTokenCategory, TPosition, TLabel, U])
    : GrammarResult[TToken, TTokenCategory, TPosition, TLabel, U] =
      new GrammarResultSuspend[TToken, TTokenCategory, TPosition, TLabel, U] {
        override val hasCut: Boolean = hasCutValue

        override def continue(tokens: NonEmptyChunk[WithLocation[TToken, TPosition]])
        : GrammarResult[TToken, TTokenCategory, TPosition, TLabel, U] =
          GrammarResultSuspend.this.continue(tokens).transformComplete(f)

        override def completeResult(pos: TPosition): GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, U] =
          GrammarResultSuspend.this.completeResult(pos).transformComplete(f).completeResult(pos)

      }

  }

  sealed trait ParseOptions[TToken, TTokenCategory, TPosition, TLabel[_]] {
    val factory: GrammarFactory[TToken, TTokenCategory, TPosition, TLabel]

    def notLeftRec: ParseOptions[TToken, TTokenCategory, TPosition, TLabel]
  }

  object ParseOptions {

    def apply[TToken, TTokenCategory, TPosition, TLabel[_], T]
      (
        leftRecRules: Set[Grammar[TToken, TTokenCategory, TPosition, TLabel, ?]],
        factory: GrammarFactory[TToken, TTokenCategory, TPosition, TLabel],
      )
      : ParseOptions[TToken, TTokenCategory, TPosition, TLabel] = {
      val factory2 = factory

      new ParseOptions[TToken, TTokenCategory, TPosition, TLabel] {
        override val factory: GrammarFactory[TToken, TTokenCategory, TPosition, TLabel] = factory2

        override def notLeftRec: ParseOptions[TToken, TTokenCategory, TPosition, TLabel] =
          if leftRecRules.isEmpty then
            this
          else
            ParseOptions(leftRecRules = Set.empty, factory = factory)
      }
    }

  }

  object Operators {

    trait GrammarConcatCombiner[A, B, T] {
      def combine(a: A, b: B): T
    }

    extension[TToken, TTokenCategory, TPosition: Order, TLabel[_], T](grammar1: => Grammar[TToken, TTokenCategory, TPosition, TLabel, T])

      def -->[U](f: T => U): Grammar[TToken, TTokenCategory, TPosition, TLabel, U] = -+>(WithLocation.lift(f))

      def -+>[U](f: WithLocation[T, TPosition] => WithLocation[U, TPosition]): Grammar[TToken, TTokenCategory, TPosition, TLabel, U] =
        new MapGrammar[TToken, TTokenCategory, TPosition, TLabel, T, U](grammar1, _.map { res => GrammarResultSuccess(res.tokens, f(res.value)) })

      def |[U](grammar2: => Grammar[TToken, TTokenCategory, TPosition, TLabel, U])
        : Grammar[TToken, TTokenCategory, TPosition, TLabel, T | U] =
        new UnionGrammar[TToken, TTokenCategory, TPosition, TLabel, T | U](grammar1, grammar2)

      def ++[U, V](grammar2: => Grammar[TToken, TTokenCategory, TPosition, TLabel, U])
        (using combiner: GrammarConcatCombiner[T, U, V])
        : Grammar[TToken, TTokenCategory, TPosition, TLabel, V] =
        ConcatGrammar(grammar1, grammar2)(isCutConcat = false) { (a, b) =>
          WithLocation(combiner.combine(a.value, b.value), Location.merge(a.location, b.location))
        }

      def ++![U, V](grammar2: => Grammar[TToken, TTokenCategory, TPosition, TLabel, U])
        (using combiner: GrammarConcatCombiner[T, U, V])
        : Grammar[TToken, TTokenCategory, TPosition, TLabel, V] =
        ConcatGrammar(grammar1, CutGrammar(grammar2))(isCutConcat = true) { (a, b) =>
          WithLocation(combiner.combine(a.value, b.value), Location.merge(a.location, b.location))
        }

      def discard: Grammar[TToken, TTokenCategory, TPosition, TLabel, Unit] = --> { _ => () }

      def ? : Grammar[TToken, TTokenCategory, TPosition, TLabel, Option[T]] =
        -->(Some.apply) | EmptyStrGrammar(None)

      def +~ : Grammar[TToken, TTokenCategory, TPosition, TLabel, NonEmptyChunk[T]] = {
        lazy val grammar1Cached = grammar1
        grammar1Cached ++ grammar1Cached.* --> { case (head, tail) => NonEmptyChunk(head, tail*) }
      }

      def * : Grammar[TToken, TTokenCategory, TPosition, TLabel, Chunk[T]] = new RepeatGrammar(grammar1)

      def observeLocation: Grammar[TToken, TTokenCategory, TPosition, TLabel, WithLocation[T, TPosition]] =
        -+> {
          case value @ WithLocation(_, location) => WithLocation(value, location)
        }

    end extension

    given [A] => GrammarConcatCombiner[A, Unit, A] = (a, _) => a
    given [B] => NotGiven[B <:< Unit] => GrammarConcatCombiner[Unit, B, B] = (_, b) => b
    given [A, B] => NotGiven[A <:< Tuple] => NotGiven[A <:< Unit] => NotGiven[B <:< Unit] => GrammarConcatCombiner[A, B, (A, B)] = (_, _)
    given [A <: Tuple, B] => NotGiven[B <:< Unit] => GrammarConcatCombiner[A, B, Tuple.Append[A, B]] = _ :* _

  }

  def token[TToken <: Matchable, TPosition, TLabel[_], TTokenCategory](category: TTokenCategory, tokenMatches: TToken => Boolean)
    : Grammar[TToken, TTokenCategory, TPosition, TLabel, TToken] = matcher(category, (t: TToken) => Some(t).filter(tokenMatches))

  def matcher[TToken <: Matchable, TPosition, TLabel[_], TTokenCategory, Result]
    (category: TTokenCategory, tokenMatcher: TToken => Option[Result])
    : Grammar[TToken, TTokenCategory, TPosition, TLabel, Result] =
    matcher(category, TokenMatcher.Anything(tokenMatcher))

  def partialMatcher[TToken <: Matchable, TPosition, TLabel[_], TTokenCategory, Result](category: TTokenCategory)
    (f: PartialFunction[TToken, Result])
    : Grammar[TToken, TTokenCategory, TPosition, TLabel, Result] = matcher(category, f.lift)

  def matcher[TToken <: Matchable, TPosition, TLabel[_], TTokenCategory, Result]
    (category: TTokenCategory, tokenMatcher: TokenMatcher[TToken, Result])
    : Grammar[TToken, TTokenCategory, TPosition, TLabel, Result] = TokenGrammar(category, tokenMatcher)

  def parseAll[TToken, TTokenCategory, TPosition, TLabel[_], T](factory: GrammarFactory[TToken, TTokenCategory, TPosition, TLabel])
    (label: TLabel[T])
    : ZChannel[Any, Nothing, Chunk[WithLocation[TToken, TPosition]], TPosition, GrammarError[TToken, TTokenCategory, TPosition], Chunk[T], TPosition] =
    sealed trait ParseStep
    case object ParseStepReady extends ParseStep
    final case class ParseStepPartial(suspend: GrammarResultSuspend[TToken, TTokenCategory, TPosition, TLabel, T]) extends ParseStep

    val rule = factory(label)
    val defaultParseOptions: ParseOptions[TToken, TTokenCategory, TPosition, TLabel] = ParseOptions(Set.empty, factory)

    def runParseStepMulti(s: ParseStep, tokens: NonEmptyChunk[WithLocation[TToken, TPosition]], acc: Chunk[T])
      : IO[GrammarError[TToken, TTokenCategory, TPosition], (ParseStep, Chunk[T])] =
      runParseStep(s, tokens) match {
        case GrammarResultSuccess(remaining, WithLocation(value, _)) =>
          NonEmptyChunk.fromChunk(remaining) match {
            case Some(remaining) => runParseStepMulti(ParseStepReady, remaining, acc :+ value)
            case None => ZIO.succeed((ParseStepReady, acc :+ value))
          }

        case GrammarResultFailure(failure) => ZIO.failCause(ZIOErrorUtil.multiCauseChunk(failure))
        case GrammarResultError(error) => ZIO.failCause(ZIOErrorUtil.multiCauseChunk(error))
        case suspend: GrammarResultSuspend[TToken, TTokenCategory, TPosition, TLabel, T] =>
          ZIO.succeed((ParseStepPartial(suspend), acc))
      }

    def runParseStep(s: ParseStep, tokens: NonEmptyChunk[WithLocation[TToken, TPosition]])
      : GrammarResult[TToken, TTokenCategory, TPosition, TLabel, T] =
      s match {
        case _: ParseStepReady.type => rule.parseTokens(tokens, defaultParseOptions)
        case ParseStepPartial(suspend) => suspend.continue(tokens)
      }

    def consumeTokens(state: ParseStep)(tokens: Chunk[WithLocation[TToken, TPosition]])
      : ZChannel[Any, Nothing, Chunk[WithLocation[TToken, TPosition]], TPosition, GrammarError[TToken, TTokenCategory, TPosition], Chunk[T], TPosition] =
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
      : ZChannel[Any, Any, Any, Any, GrammarError[TToken, TTokenCategory, TPosition], Chunk[T], TPosition] =
      state match {
        case _: ParseStepReady.type => ZChannel.succeed(end)
        case ParseStepPartial(suspend) =>
          def parseEnd(result: GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, T], acc: Chunk[T])
            : ZChannel[Any, Any, Any, Any, GrammarError[TToken, TTokenCategory, TPosition], Chunk[T], TPosition] =
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
      : ZChannel[Any, Nothing, Chunk[WithLocation[TToken, TPosition]], TPosition, GrammarError[TToken, TTokenCategory, TPosition], Chunk[T], TPosition] =
      ZChannel.readWithCause(
        in = consumeTokens(state),
        halt = ZChannel.failCause(_),
        done = finish(state),
      )

    parse(ParseStepReady)

  end parseAll

  private final case class EmptyStrGrammar[TToken, TTokenCategory, TPosition, TLabel[_], T](result: T)
      extends Grammar[TToken, TTokenCategory, TPosition, TLabel, T] {

    override def parseTokens(tokens: NonEmptyChunk[WithLocation[TToken, TPosition]], options: TParseOptions)
      : GrammarResult[TToken, TTokenCategory, TPosition, TLabel, T] =
      GrammarResultSuccess(
        tokens,
        WithLocation(result, Location(options.factory.fileName, tokens.head.location.start, tokens.head.location.start)),
      )

    override def parseEnd(pos: TPosition, options: TParseOptions)
      : GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, T] =
      GrammarResultSuccess(Chunk(), WithLocation(result, Location(options.factory.fileName, pos, pos)))

  }

  private final case class TokenGrammar[TToken <: Matchable, TTokenCategory, TPosition, TLabel[_], T]
    (
      category: TTokenCategory,
      tokenMatcher: TokenMatcher[TToken, T],
    ) extends Grammar[TToken, TTokenCategory, TPosition, TLabel, T] {

    override def parseTokens(tokens: NonEmptyChunk[WithLocation[TToken, TPosition]], options: TParseOptions)
      : GrammarResult[TToken, TTokenCategory, TPosition, TLabel, T] = {

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
          GrammarResultFailure(NonEmptyChunk(GrammarError.UnexpectedToken(category, token)))
      }
    }

    override def parseEnd(pos: TPosition, options: TParseOptions)
      : GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, T] =
      GrammarResultFailure(NonEmptyChunk(GrammarError.UnexpectedEndOfFile(category, options.factory.fileName, pos)))

  }

  private final class ConcatGrammar[TToken, TTokenCategory, TPosition, TLabel[_], A, B, T]
    (
      grammarAUncached: => Grammar[TToken, TTokenCategory, TPosition, TLabel, A],
      grammarBUncached: => Grammar[TToken, TTokenCategory, TPosition, TLabel, B],
    )(
      isCutConcat: Boolean
    )(
      combine: (WithLocation[A, TPosition], WithLocation[B, TPosition]) => WithLocation[T, TPosition],
    ) extends Grammar[TToken, TTokenCategory, TPosition, TLabel, T] {

    private lazy val grammarA = grammarAUncached
    private lazy val grammarB = grammarBUncached

    private def nextHandler(valueA: WithLocation[A, TPosition], options: TParseOptions)
      : GrammarResultSuspend[TToken, TTokenCategory, TPosition, TLabel, T] =
      new GrammarResultSuspend[TToken, TTokenCategory, TPosition, TLabel, T] {

        override val hasCut: Boolean = isCutConcat

        override def continue(tokens: NonEmptyChunk[WithLocation[TToken, TPosition]])
          : GrammarResult[TToken, TTokenCategory, TPosition, TLabel, T] =
          grammarB.parseTokens(tokens, options.notLeftRec).treatFailureAsErrorWhen(hasCut).map {
            case GrammarResultSuccess(tokens3, valueB) =>
              GrammarResultSuccess(tokens3, combine(valueA, valueB))
          }

        override def completeResult(pos: TPosition): GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, T] =
          grammarB.parseEnd(pos, options.notLeftRec).treatFailureAsErrorWhen(hasCut).map {
            case GrammarResultSuccess(state3, valueB) =>
              GrammarResultSuccess(state3, combine(valueA, valueB))
          }

      }

    override def parseTokens(tokens: NonEmptyChunk[WithLocation[TToken, TPosition]], options: TParseOptions)
      : GrammarResult[TToken, TTokenCategory, TPosition, TLabel, T] =
      grammarA.parseTokens(tokens, options).flatMap {
        case GrammarResultSuccess(ChunkUnCons(nextTokens), valueA) =>
          nextHandler(valueA, options).continue(nextTokens)

        case GrammarResultSuccess(_, valueA) =>
          nextHandler(valueA, options)
      }

    override def parseEnd(pos: TPosition, options: TParseOptions)
      : GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, T] =
      grammarA.parseEnd(pos, options).flatMap[T] {
        case GrammarResultSuccess(ChunkUnCons(nextTokens), valueA) =>
          nextHandler(valueA, options).continue(nextTokens).completeResult(pos)

        case GrammarResultSuccess(_, valueA) =>
          nextHandler(valueA, options).completeResult(pos)
      }

  }

  private final class CutGrammar[TToken, TTokenCategory, TPosition, TLabel[_], T]
    (
      innerUncached: => Grammar[TToken, TTokenCategory, TPosition, TLabel, T]
    ) extends Grammar[TToken, TTokenCategory, TPosition, TLabel, T] {

    private lazy val inner = innerUncached

    override def parseTokens(tokens: NonEmptyChunk[WithLocation[TToken, TPosition]], options: TParseOptions)
      : GrammarResult[TToken, TTokenCategory, TPosition, TLabel, T] = inner.parseTokens(tokens, options).treatFailureAsError

    override def parseEnd(pos: TPosition, options: TParseOptions)
      : GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, T] = inner.parseEnd(pos, options).treatFailureAsError

  }

  private object CutGrammar {

    def apply[TToken, TTokenCategory, TPosition, TLabel[_], T]
      (
        inner: => Grammar[TToken, TTokenCategory, TPosition, TLabel, T]
      )
      : CutGrammar[TToken, TTokenCategory, TPosition, TLabel, T] = new CutGrammar(inner)

  }

  private final class UnionGrammar[TToken, TTokenCategory, TPosition: Order, TLabel[_], T]
    (
      grammarAUncached: => Grammar[TToken, TTokenCategory, TPosition, TLabel, T],
      grammarBUncached: => Grammar[TToken, TTokenCategory, TPosition, TLabel, T],
    )
      extends Grammar[TToken, TTokenCategory, TPosition, TLabel, T] {

    private lazy val grammarA = grammarAUncached
    private lazy val grammarB = grammarBUncached


    private def findLastErrorPos(errorList: TErrorList): TPosition =
      errorList.maximumBy(_.location.end).location.end

    override def parseTokens(tokens: NonEmptyChunk[WithLocation[TToken, TPosition]], options: TParseOptions)
      : GrammarResult[TToken, TTokenCategory, TPosition, TLabel, T] =
      grammarA.parseTokens(tokens, options).recoverFailure {
        case (laterTokens, GrammarResultFailure(errorListA)) =>
          grammarB.parseTokens(tokens ++ laterTokens, options).transformComplete {
            case result @ GrammarResultSuccess(_, _) => result
            case result @ GrammarResultError(_) => result
            case GrammarResultFailure(errorListB) =>
              GrammarResultFailure(
                Order[TPosition].compare(
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
      : GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, T] =
      grammarA.parseEnd(pos, options) match {
        case result @ GrammarResultSuccess(_, _) => result
        case result @ GrammarResultError(_) => result
        case GrammarResultFailure(errorListA) =>
          grammarB.parseEnd(pos, options) match {
            case result @ GrammarResultSuccess(_, _) => result
            case result @ GrammarResultError(_) => result
            case GrammarResultFailure(errorListB) =>
              GrammarResultFailure(
                Order[TPosition].compare(
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

    def fromList[TToken, TTokenCategory, TPosition: Order, TLabel[_], T]
      (grammars: NonEmptyChunk[Lazy[Grammar[TToken, TTokenCategory, TPosition, TLabel, T]]])
      : Grammar[TToken, TTokenCategory, TPosition, TLabel, T] =
      grammars.reduceLeft { (a, b) =>
        Lazy(UnionGrammar(a.value, b.value))
      }.value

  }

  private final class RepeatGrammar[TToken, TTokenCategory, TPosition, TLabel[_], T]
    (innerUncached: => Grammar[TToken, TTokenCategory, TPosition, TLabel, T])
      extends Grammar[TToken, TTokenCategory, TPosition, TLabel, Chunk[T]] {

    private lazy val inner = innerUncached

    override def parseTokens(tokens: NonEmptyChunk[WithLocation[TToken, TPosition]], options: TParseOptions)
      : GrammarResult[TToken, TTokenCategory, TPosition, TLabel, Chunk[T]] = parseInner(tokens, options, Chunk.empty)

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
      : GrammarResult[TToken, TTokenCategory, TPosition, TLabel, Chunk[T]] =
      inner.parseTokens(tokens, options).flatMap {
        case GrammarResultSuccess(ChunkUnCons(remaining), item) =>
          parseInner(remaining, options.notLeftRec, items :+ item)

        case GrammarResultSuccess(_, item) =>
          new GrammarResultSuspend[TToken, TTokenCategory, TPosition, TLabel, Chunk[T]] {
            override val hasCut: Boolean = false

            override def continue(tokens: NonEmptyChunk[WithLocation[TToken, TPosition]])
              : GrammarResult[TToken, TTokenCategory, TPosition, TLabel, Chunk[T]] =
              parseInner(tokens, options.notLeftRec, items :+ item)

            override def completeResult(pos: TPosition)
              : GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, Chunk[T]] =
              GrammarResultSuccess(Chunk(), finalItems(items :+ item, options.factory.fileName, pos))

          }
      }
        .recoverFailure { (laterTokens, _) =>
          GrammarResultSuccess(tokens ++ laterTokens, finalItems(items, options.factory.fileName, tokens.head.location.start))
        }

    override def parseEnd(pos: TPosition, options: TParseOptions)
      : GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, Chunk[T]] =
      GrammarResultSuccess(Chunk(), WithLocation(Chunk.empty, Location(options.factory.fileName, pos, pos)))

    private def removeSource[A](ws: WithLocation[A, TPosition]): A = ws.value
  }

  private final class MapGrammar[TToken, TTokenCategory, TPosition, TLabel[_], T, U]
    (
      innerUncached: => Grammar[TToken, TTokenCategory, TPosition, TLabel, T],
      f: GrammarResult[TToken, TTokenCategory, TPosition, TLabel, T] => GrammarResult[TToken, TTokenCategory, TPosition, TLabel, U],
    ) extends Grammar[TToken, TTokenCategory, TPosition, TLabel, U] {

    private lazy val inner = innerUncached

    override def parseTokens(tokens: NonEmptyChunk[WithLocation[TToken, TPosition]], options: TParseOptions)
      : GrammarResult[TToken, TTokenCategory, TPosition, TLabel, U] = f(inner.parseTokens(tokens, options))

    override def parseEnd(pos: TPosition, options: TParseOptions)
      : GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, U] = f(inner.parseEnd(pos, options)).completeResult(pos)

  }

  private final class LabelRefGrammar[TToken, TTokenCategory, TPosition, TLabel[_], T](label: TLabel[T])
      extends Grammar[TToken, TTokenCategory, TPosition, TLabel, T] {

    override def parseTokens(tokens: NonEmptyChunk[WithLocation[TToken, TPosition]], options: TParseOptions)
      : GrammarResult[TToken, TTokenCategory, TPosition, TLabel, T] = options.factory(label).parseTokens(tokens, options)

    override def parseEnd(pos: TPosition, options: TParseOptions)
      : GrammarResultComplete[TToken, TTokenCategory, TPosition, TLabel, T] = options.factory(label).parseEnd(pos, options)

  }
}
