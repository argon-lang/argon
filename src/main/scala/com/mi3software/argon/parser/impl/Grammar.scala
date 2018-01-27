package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser.impl.Grammar.{ConcatGrammar, EmptyStrGrammar, GrammarError, UnionGrammar}
import com.mi3software.argon.util.{FilePosition, SourceLocation, WithSource}

import scala.language.postfixOps
import scalaz._
import Scalaz._
import scalaz.Leibniz.===

sealed trait Grammar[TToken, TokenCategory, T] {

  protected type EndOfInputResult = NonEmptyList[GrammarError[TToken, TokenCategory]] \/ NonEmptyList[WithSource[T]]

  def derive(token: WithSource[TToken]): Grammar[TToken, TokenCategory, T]
  def endOfInput(pos: FilePosition): EndOfInputResult

  def shortCircuit: Boolean = false

  final def map[U](f: T => U): Grammar[TToken, TokenCategory, U] = mapSource(WithSource.lift(f))

  def mapSource[U](f: WithSource[T] => WithSource[U]): Grammar[TToken, TokenCategory, U]


  final def ++[U](b: => Grammar[TToken, TokenCategory, U]): Grammar[TToken, TokenCategory, (T, U)] =
    ConcatGrammar(this, b) { (a, b) => WithSource((a.value, b.value), SourceLocation.merge(a.location, b.location)) }

  def |(other: Grammar[TToken, TokenCategory, T]): Grammar[TToken, TokenCategory, T] =
    other match {
      case UnionGrammar(grammars) =>
        UnionGrammar.createSimplified(this <:: grammars)

      case _ =>
        UnionGrammar(NonEmptyList(this, other))
    }

  def discard: Grammar[TToken, TokenCategory, Unit] = this.map { _ => () }
  def ? : Grammar[TToken, TokenCategory, Option[T]] =
    this.map[Option[T]](Some.apply) | EmptyStrGrammar(NonEmptyList(WithSource(None, SourceLocation.empty)))

  def + : Grammar[TToken, TokenCategory, NonEmptyList[T]] = (this ++ (this*)).map { case (head, tail) => NonEmptyList.nel(head, tail) }
  def * : Grammar[TToken, TokenCategory, IList[T]] = ((this ++ (this*))?).map {
    case Some((head, tail)) => head +: tail
    case None => IList()
  }

  def observeSource: Grammar[TToken, TokenCategory, WithSource[T]] = mapSource {
    case value @ WithSource(_, location) => WithSource(value, location)
  }

  def consumeResults[A](implicit ev: T === IList[A]): (IList[A], Grammar[TToken, TokenCategory, T]) =
    (INil(), this)
}

object Grammar {

  def emptyString[TToken, TokenCategory, T](value: T): Grammar[TToken, TokenCategory, T] =
    EmptyStrGrammar(NonEmptyList(WithSource(value, SourceLocation.empty)))

  def token[TToken, TokenCategory](category: TokenCategory, tokenMatches: TToken => Boolean): Grammar[TToken, TokenCategory, TToken] =
    matcher(category, t => Some(t).filter(tokenMatches))

  def tokenSource[TToken, TokenCategory](category: TokenCategory, tokenMatches: WithSource[TToken] => Boolean): Grammar[TToken, TokenCategory, TToken] =
    matcherSource(category, t => Some(t).filter(tokenMatches))

  def matcher[TToken, TokenCategory, Result](category: TokenCategory, tokenMatcher: TToken => Option[Result]): Grammar[TToken, TokenCategory, Result] =
    matcherSource(category, WithSource.liftF(tokenMatcher))

  def matcherSource[TToken, TokenCategory, Result](category: TokenCategory, tokenMatcher: TokenMatcher[TToken, Result]): Grammar[TToken, TokenCategory, Result] =
    TokenGrammar(category, tokenMatcher)

  sealed trait GrammarError[TToken, TokenCategory] {
    def location: SourceLocation
  }

  final case class ExpectedEndOfFile[TToken, TokenCategory](token: WithSource[TToken]) extends GrammarError[TToken, TokenCategory] {
    override def location: SourceLocation = token.location
  }

  final case class UnexpectedToken[TToken, TokenCategory](expectedCategory: TokenCategory, token: WithSource[TToken]) extends GrammarError[TToken, TokenCategory] {
    override def location: SourceLocation = token.location
  }

  final case class UnexpectedEndOfFile[TToken, TokenCategory](expectedCategory: TokenCategory, position: FilePosition) extends GrammarError[TToken, TokenCategory] {
    override def location: SourceLocation = SourceLocation(position, FilePosition(position.line, position.position + 1))
  }

  type TokenMatcher[TToken, T] = WithSource[TToken] => Option[WithSource[T]]

  private def concatRepeater[TToken, TokenCategory, T](item: Grammar[TToken, TokenCategory, T]): Grammar[TToken, TokenCategory, IList[T]] = {
    lazy val repeater: Grammar[TToken, TokenCategory, IList[T]] = ((item ++ repeater)?).map {
      case Some((head, tail)) => head +: tail
      case None => INil()
    }

    repeater
  }


  private final case class RejectGrammar[TToken, TokenCategory, T](grammarErrors: NonEmptyList[GrammarError[TToken, TokenCategory]]) extends Grammar[TToken, TokenCategory, T] {
    override def derive(token: WithSource[TToken]): Grammar[TToken, TokenCategory, T] = RejectGrammar(grammarErrors)
    override def endOfInput(pos: FilePosition): EndOfInputResult = -\/(grammarErrors)

    override def shortCircuit: Boolean = true

    override def mapSource[U](f: WithSource[T] => WithSource[U]): Grammar[TToken, TokenCategory, U] =
      RejectGrammar(grammarErrors)
  }

  private final case class EmptyStrGrammar[TToken, TokenCategory, T](result: NonEmptyList[WithSource[T]]) extends Grammar[TToken, TokenCategory, T] {
    override def derive(token: WithSource[TToken]): Grammar[TToken, TokenCategory, T] = RejectGrammar(NonEmptyList(ExpectedEndOfFile(token)))
    override def endOfInput(pos: FilePosition): EndOfInputResult = \/-(result)

    override def mapSource[U](f: WithSource[T] => WithSource[U]): Grammar[TToken, TokenCategory, U] =
      EmptyStrGrammar(result.map(f))
  }

  private final case class TokenGrammar[TToken, TokenCategory, T](category: TokenCategory, tokenMatcher: TokenMatcher[TToken, T]) extends Grammar[TToken, TokenCategory, T] {
    override def derive(token: WithSource[TToken]): Grammar[TToken, TokenCategory, T] =
      tokenMatcher(token)
        .fold(
          RejectGrammar(NonEmptyList(UnexpectedToken(category, token))) : Grammar[TToken, TokenCategory, T]
        ) { res =>
          EmptyStrGrammar(NonEmptyList(res))
        }

    override def endOfInput(pos: FilePosition): EndOfInputResult =
      -\/(NonEmptyList(UnexpectedEndOfFile(category, pos)))

    override def mapSource[U](f: WithSource[T] => WithSource[U]): Grammar[TToken, TokenCategory, U] =
      TokenGrammar(category, tokenMatcher andThen f.lift)
  }

  private final class ConcatGrammar[TToken, TokenCategory, A, B, T]
  (
    grammarA: Grammar[TToken, TokenCategory, A],
    grammarBUncached: => Grammar[TToken, TokenCategory, B],
    combine: (WithSource[A], WithSource[B]) => WithSource[T]
  ) extends Grammar[TToken, TokenCategory, T] {

    private lazy val grammarB = grammarBUncached

    override def derive(token: WithSource[TToken]): Grammar[TToken, TokenCategory, T] =
      grammarA.endOfInput(token.location.start) match {
        case \/-(items) =>
          UnionGrammar.createSimplified(NonEmptyList(

            ConcatGrammar(grammarA.derive(token), grammarB)(combine),

            UnionGrammar.createSimplified(
              items.map { a =>
                grammarB.derive(token).mapSource { b =>
                  combine(a, b)
                }
              }
            ),

          ))

        case -\/(_) =>
          ConcatGrammar(grammarA.derive(token), grammarB)(combine)
      }

    override def endOfInput(pos: FilePosition): EndOfInputResult =
      for {
        aItems <- grammarA.endOfInput(pos)
        bItems <- grammarB.endOfInput(pos)
      } yield for {
        a <- aItems
        b <- bItems
      } yield combine(a, b)

    override def mapSource[U](f: WithSource[T] => WithSource[U]): Grammar[TToken, TokenCategory, U] =
      ConcatGrammar(grammarA, grammarB)((a, b) => f(combine(a, b)))

  }

  private object ConcatGrammar {
    def apply[TToken, TokenCategory, A, B, T]
    (
      grammarA: Grammar[TToken, TokenCategory, A],
      grammarB: => Grammar[TToken, TokenCategory, B]
    )(
      combine: (WithSource[A], WithSource[B]) => WithSource[T]
    ): ConcatGrammar[TToken, TokenCategory, A, B, T] =
      new ConcatGrammar(grammarA, grammarB, combine)


    def createSimplified[TToken, TokenCategory, A, B, T]
    (
      grammarA: Grammar[TToken, TokenCategory, A],
      grammarB: => Grammar[TToken, TokenCategory, B]
    )(
      combine: (WithSource[A], WithSource[B]) => WithSource[T]
    ): Grammar[TToken, TokenCategory, T] =
      grammarA match {
        case RejectGrammar(grammarErrors) => RejectGrammar(grammarErrors)
        case EmptyStrGrammar(result) =>
          UnionGrammar.createSimplified(
            result.map { a =>
              grammarB.mapSource(b => combine(a, b))
            }
          )

        case _ =>
          apply(grammarA, grammarB)(combine)
      }

  }

  private final case class UnionGrammar[TToken, TokenCategory, T](grammars: NonEmptyList[Grammar[TToken, TokenCategory, T]]) extends Grammar[TToken, TokenCategory, T] {
    override def derive(token: WithSource[TToken]): Grammar[TToken, TokenCategory, T] =
      UnionGrammar.createSimplified(
        grammars
          .map(_.derive(token))
      )

    override def endOfInput(pos: FilePosition): EndOfInputResult =
      combineResults(grammars.map(_.endOfInput(pos)))

    override def |(other: Grammar[TToken, TokenCategory, T]): Grammar[TToken, TokenCategory, T] =
      other match {
        case other: UnionGrammar[TToken, TokenCategory, T] =>
          UnionGrammar(grammars.append(other.grammars))

        case _ =>
          UnionGrammar((other <:: grammars.reverse).reverse)
      }

    private def combineResults(results: NonEmptyList[EndOfInputResult]): EndOfInputResult = {

      def handleSuccess(successes: NonEmptyList[WithSource[T]], results: IList[EndOfInputResult]): EndOfInputResult =
        \/-(successes :::> results.collect { case \/-(result) => result.list }.flatten)

      def handleFailure(errors: NonEmptyList[GrammarError[TToken, TokenCategory]], results: IList[EndOfInputResult]): EndOfInputResult =
        results match {
          case ICons(-\/(newErrors), tail) =>
            handleFailure(errors.append(newErrors), tail)

          case ICons(\/-(success), tail) =>
            handleSuccess(success, tail)

          case INil() =>
            -\/(errors)
        }

      results.head match {
        case -\/(errors) => handleFailure(errors, results.tail)
        case \/-(successes) => handleSuccess(successes, results.tail)
      }
    }

    override def mapSource[U](f: WithSource[T] => WithSource[U]): Grammar[TToken, TokenCategory, U] =
      UnionGrammar(grammars.map(_.mapSource(f)))
  }

  private object UnionGrammar {
    def createSimplified[TToken, TokenCategory, T](grammars: NonEmptyList[Grammar[TToken, TokenCategory, T]]): Grammar[TToken, TokenCategory, T] = {
      val allGrammars = grammars.flatMap {
        case UnionGrammar(inner) => inner
        case inner => NonEmptyList(inner)
      }

      allGrammars match {
        case NonEmptyList(head, INil()) => head
        case NonEmptyList(head, tail) => UnionGrammar(NonEmptyList.nel(head, tail))
      }
    }
  }

  sealed trait ListGrammar[TToken, TokenCategory, T] extends Grammar[TToken, TokenCategory, IList[T]] {

    override def consumeResults[A](implicit ev: IList[T] === IList[A]): (IList[A], Grammar[TToken, TokenCategory, IList[T]]) = {
      val (list, newGrammar) = consumeResultsImpl
      (ev(list), newGrammar)
    }

    def consumeResultsImpl: (IList[T], ListGrammar[TToken, TokenCategory, T])

  }

  final case class RepeatGrammar[TToken, TokenCategory, T]
  (
    items: WithSource[IList[T]],
    item: Grammar[TToken, TokenCategory, T]
  ) extends ListGrammar[TToken, TokenCategory, T] {
    override def derive(token: WithSource[TToken]): Grammar[TToken, TokenCategory, IList[T]] =
      RepeatBuilderGrammar(items, item.derive(token), item)

    override def endOfInput(pos: FilePosition): EndOfInputResult =
      \/-(NonEmptyList(items))

    override def mapSource[U](f: WithSource[IList[T]] => WithSource[U]): Grammar[TToken, TokenCategory, U] =
      concatRepeater(item).mapSource(f)

    override def consumeResultsImpl: (IList[T], ListGrammar[TToken, TokenCategory, T]) = {
      val newLocation = SourceLocation(items.location.end, items.location.end)
      (items.value, RepeatGrammar(WithSource(INil(), newLocation), item))
    }

  }

  final case class RepeatBuilderGrammar[TToken, TokenCategory, T]
  (
    items: WithSource[IList[T]],
    builder: Grammar[TToken, TokenCategory, T],
    item: Grammar[TToken, TokenCategory, T]
  ) extends ListGrammar[TToken, TokenCategory, T] {
    override def derive(token: WithSource[TToken]): Grammar[TToken, TokenCategory, IList[T]] =
      builder.endOfInput(token.location.start) match {
        case \/-(builderItems) =>
          UnionGrammar.createSimplified(NonEmptyList(

            ConcatGrammar.createSimplified(builder.derive(token), concatRepeater(item)) { (a, b) =>
              WithSource(
                items.value ++ (a.value +: b.value),
                SourceLocation.merge(items.location, b.location)
              )
            },

            UnionGrammar.createSimplified(
              builderItems.map { builderItem =>
                RepeatGrammar(WithSource(items.value :+ builderItem.value, SourceLocation.merge(items.location, builderItem.location)), item)
              }
            ),

          ))

        case -\/(_) =>
          RepeatBuilderGrammar(items, builder.derive(token), item)
      }


    override def endOfInput(pos: FilePosition): EndOfInputResult =
      builder.endOfInput(pos).map { builderResults =>
        builderResults.map { builderResult =>
          WithSource(items.value :+ builderResult.value, SourceLocation.merge(items.location, builderResult.location))
        }
      }

    override def mapSource[U](f: WithSource[IList[T]] => WithSource[U]): Grammar[TToken, TokenCategory, U] =
      concatRepeater(item).mapSource { listResult =>
        f(WithSource(items.value ++ listResult.value, SourceLocation.merge(items.location, listResult.location)))
      }

    override def consumeResultsImpl: (IList[T], ListGrammar[TToken, TokenCategory, T]) = {
      val newLocation = SourceLocation(items.location.end, items.location.end)
      (items.value, RepeatBuilderGrammar(WithSource(INil(), newLocation), builder, item))
    }


  }

}
