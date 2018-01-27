package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser.impl.Grammar.{ConcatGrammar, EmptyStrGrammar, GrammarError, UnionGrammar}
import com.mi3software.argon.util.{FilePosition, SourceLocation, WithSource}

import scala.language.postfixOps
import scalaz._
import Scalaz._

sealed trait Grammar[TToken, TokenCategory, T] {

  protected type EndOfInputResult = NonEmptyList[GrammarError[TToken, TokenCategory]] \/ NonEmptyList[WithSource[T]]

  def derive(token: WithSource[TToken]): Grammar[TToken, TokenCategory, T]
  def endOfInput(pos: FilePosition): EndOfInputResult

  def shortCircuit: Boolean = false

  final def map[U](f: T => U): Grammar[TToken, TokenCategory, U] = mapSource(WithSource.lift(f))

  final def mapSource[U](f: WithSource[T] => WithSource[U]): Grammar[TToken, TokenCategory, U] = new Grammar[TToken, TokenCategory, U] {
    override def derive(token: WithSource[TToken]): Grammar[TToken, TokenCategory, U] =
      Grammar.this.derive(token).mapSource(f)

    override def endOfInput(pos: FilePosition): EndOfInputResult =
      Grammar.this.endOfInput(pos).map(_.map(f))
  }

  final def ++[U](b: => Grammar[TToken, TokenCategory, U]): Grammar[TToken, TokenCategory, (T, U)] = ConcatGrammar(this, b)
  def |(other: Grammar[TToken, TokenCategory, T]): Grammar[TToken, TokenCategory, T] =
    other match {
      case other: UnionGrammar[TToken, TokenCategory, T] =>
        UnionGrammar(this <:: other.grammars)

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

  private def createSimplifiedUnion[TToken, TokenCategory, T](grammars: NonEmptyList[Grammar[TToken, TokenCategory, T]]): Grammar[TToken, TokenCategory, T] =
    createListUnion(
      grammars.list.filter {
        case RejectGrammar(_) => false
        case _ => true
      } match {
        case ICons(head, tail) => NonEmptyList.nel(head, tail)
        case INil() => grammars
      }
    )


  private def createListUnion[TToken, TokenCategory, T](grammars: NonEmptyList[Grammar[TToken, TokenCategory, T]]): Grammar[TToken, TokenCategory, T] =
    grammars match {
      case NonEmptyList(head, INil()) => head
      case NonEmptyList(head, tail) => UnionGrammar(NonEmptyList.nel(head, tail))
    }


  private final case class RejectGrammar[TToken, TokenCategory, T](grammarErrors: NonEmptyList[GrammarError[TToken, TokenCategory]]) extends Grammar[TToken, TokenCategory, T] {
    override def derive(token: WithSource[TToken]): Grammar[TToken, TokenCategory, T] = RejectGrammar(grammarErrors)
    override def endOfInput(pos: FilePosition): EndOfInputResult = -\/(grammarErrors)
  }

  private final case class EmptyStrGrammar[TToken, TokenCategory, T](result: NonEmptyList[WithSource[T]]) extends Grammar[TToken, TokenCategory, T] {
    override def derive(token: WithSource[TToken]): Grammar[TToken, TokenCategory, T] = RejectGrammar(NonEmptyList(ExpectedEndOfFile(token)))
    override def endOfInput(pos: FilePosition): EndOfInputResult = \/-(result)
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
  }

  private final class ConcatGrammar[TToken, TokenCategory, A, B](grammarA: Grammar[TToken, TokenCategory, A], grammarBUncached: => Grammar[TToken, TokenCategory, B]) extends Grammar[TToken, TokenCategory, (A, B)] {

    private lazy val grammarB = grammarBUncached

    override def derive(token: WithSource[TToken]): Grammar[TToken, TokenCategory, (A, B)] =
      grammarA.endOfInput(token.location.start) match {
        case \/-(items) =>
          createSimplifiedUnion(NonEmptyList(

            ConcatGrammar(grammarA.derive(token), grammarB),

            createSimplifiedUnion(
              items.map { a =>
                grammarB.derive(token).mapSource { b =>
                  WithSource((a.value, b.value), SourceLocation.merge(a.location, b.location))
                }
              }
            ),

          ))

        case -\/(_) =>
          ConcatGrammar(grammarA.derive(token), grammarB)
      }

    override def endOfInput(pos: FilePosition): EndOfInputResult =
      for {
        aItems <- grammarA.endOfInput(pos)
        bItems <- grammarB.endOfInput(pos)
      } yield for {
        a <- aItems
        b <- bItems
      } yield WithSource((a.value, b.value), SourceLocation.merge(a.location, b.location))
  }

  private object ConcatGrammar {
    def apply[TToken, TokenCategory, A, B](grammarA: Grammar[TToken, TokenCategory, A], grammarB: => Grammar[TToken, TokenCategory, B]): ConcatGrammar[TToken, TokenCategory, A, B] =
      new ConcatGrammar(grammarA, grammarB)
  }

  private final case class UnionGrammar[TToken, TokenCategory, T](grammars: NonEmptyList[Grammar[TToken, TokenCategory, T]]) extends Grammar[TToken, TokenCategory, T] {
    override def derive(token: WithSource[TToken]): Grammar[TToken, TokenCategory, T] =
      createSimplifiedUnion(
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
  }

}
