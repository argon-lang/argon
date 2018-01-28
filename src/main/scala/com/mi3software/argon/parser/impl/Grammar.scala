package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser.impl.Grammar.{ConcatGrammar, EmptyStrGrammar, GrammarError, UnionGrammar}
import com.mi3software.argon.util.{FilePosition, SourceLocation, WithSource}

import scala.language.postfixOps
import scalaz._
import Scalaz._


import com.thoughtworks.each.Monadic._

sealed trait Grammar[TToken, TokenCategory, T] {

  def run[M[_] : Monad](pos: FilePosition, tokens: StreamT[M, WithSource[TToken]]): M[NonEmptyList[GrammarError[TToken, TokenCategory]] \/ (WithSource[T], StreamT[M, WithSource[TToken]])]

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


  def stream[M[_] : Monad](tokens: StreamT[M, WithSource[TToken]]): StreamT[M, NonEmptyList[GrammarError[TToken, TokenCategory]] \/ WithSource[T]] =
    StreamT[M, NonEmptyList[GrammarError[TToken, TokenCategory]] \/ WithSource[T]](
      monadic[M] {
        tokens.uncons.each match {
          case Some((WithSource(_, SourceLocation(pos, _)), _)) =>
            run(pos, tokens).each match {
              case -\/(errors) =>
                StreamT.Yield(-\/(errors), StreamT.empty)

              case \/-((item, remaining)) =>
                StreamT.Yield(\/-(item), stream(remaining))
            }

          case None =>
            StreamT.Done
        }
      }
    )

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


  private final case class RejectGrammar[TToken, TokenCategory, T](grammarErrors: NonEmptyList[GrammarError[TToken, TokenCategory]]) extends Grammar[TToken, TokenCategory, T] {


    override def run[M[_] : Monad](pos: FilePosition, tokens: StreamT[M, WithSource[TToken]]): M[NonEmptyList[GrammarError[TToken, TokenCategory]] \/ (WithSource[T], StreamT[M, WithSource[TToken]])] =
      Monad[M].point(-\/(grammarErrors))

    override def shortCircuit: Boolean = true

    override def mapSource[U](f: WithSource[T] => WithSource[U]): Grammar[TToken, TokenCategory, U] =
      RejectGrammar(grammarErrors)
  }

  private final case class EmptyStrGrammar[TToken, TokenCategory, T](result: NonEmptyList[WithSource[T]]) extends Grammar[TToken, TokenCategory, T] {

    override def run[M[_] : Monad](pos: FilePosition, tokens: StreamT[M, WithSource[TToken]]): M[NonEmptyList[GrammarError[TToken, TokenCategory]] \/ (WithSource[T], StreamT[M, WithSource[TToken]])] =
      Monad[M].point(\/-((result.head, tokens)))

    override def mapSource[U](f: WithSource[T] => WithSource[U]): Grammar[TToken, TokenCategory, U] =
      EmptyStrGrammar(result.map(f))
  }

  private final case class TokenGrammar[TToken, TokenCategory, T](category: TokenCategory, tokenMatcher: TokenMatcher[TToken, T]) extends Grammar[TToken, TokenCategory, T] {

    override def run[M[_] : Monad](pos: FilePosition, tokens: StreamT[M, WithSource[TToken]]): M[NonEmptyList[GrammarError[TToken, TokenCategory]] \/ (WithSource[T], StreamT[M, WithSource[TToken]])] =
      tokens.uncons.map {
        case Some((token, tail)) =>
          tokenMatcher(token) match {
            case Some(res) =>
              \/-((res, tail))

            case None =>
              -\/(NonEmptyList(UnexpectedToken(category, token)))
          }

        case None =>
          -\/(NonEmptyList(UnexpectedEndOfFile(category, pos)))
      }

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

    override def run[M[_] : Monad](pos: FilePosition, tokens: StreamT[M, WithSource[TToken]]): M[NonEmptyList[GrammarError[TToken, TokenCategory]] \/ (WithSource[T], StreamT[M, WithSource[TToken]])] = monadic[M] {
      grammarA.run(pos, tokens).each match {
        case -\/(errors) => -\/(errors)
        case \/-((a, tail)) =>
          grammarB.run(a.location.end, tail).each match {
            case -\/(errors) => -\/(errors)
            case \/-((b, tail2)) => \/-((combine(a, b), tail2))
          }
      }
    }

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

    override def run[M[_] : Monad](pos: FilePosition, tokens: StreamT[M, WithSource[TToken]]): M[NonEmptyList[GrammarError[TToken, TokenCategory]] \/ (WithSource[T], StreamT[M, WithSource[TToken]])] = {

      def findLastErrorPos(errorList: NonEmptyList[GrammarError[TToken, TokenCategory]]): FilePosition =
        errorList.maximumBy1(_.location.end).location.end

      def chooseBestErrorList(candidate: NonEmptyList[GrammarError[TToken, TokenCategory]], candidateLastPos: FilePosition, errorLists: IList[NonEmptyList[GrammarError[TToken, TokenCategory]]]): NonEmptyList[GrammarError[TToken, TokenCategory]] =
        errorLists match {
          case ICons(head, tail) =>
            val headLastPos = findLastErrorPos(head)
            val cmp = candidateLastPos compareTo headLastPos

            if(cmp > 0)
              chooseBestErrorList(candidate, candidateLastPos, tail)
            else if(cmp < 0)
              chooseBestErrorList(head, headLastPos, tail)
            else
              chooseBestErrorList(candidate append head, candidateLastPos, tail)

          case INil() => candidate
        }

      def handleErrorLists(errorLists: NonEmptyList[NonEmptyList[GrammarError[TToken, TokenCategory]]], grammars: IList[Grammar[TToken, TokenCategory, T]]): M[NonEmptyList[GrammarError[TToken, TokenCategory]] \/ (WithSource[T], StreamT[M, WithSource[TToken]])] =
        monadic[M] {
          grammars match {
            case ICons(head, tail) =>
              head.run(pos, tokens).each match {
                case -\/(errorList) => handleErrorLists(errorList <:: errorLists, tail).each
                case result @ \/-(_) => result
              }

            case INil() =>
              val lists = errorLists.reverse
              -\/(chooseBestErrorList(lists.head, findLastErrorPos(lists.head), lists.tail))
          }
        }

      monadic[M] {
        grammars.head.run(pos, tokens).each match {
          case -\/(errorList) => handleErrorLists(NonEmptyList(errorList), grammars.tail).each
          case result @ \/-(_) => result
        }
      }

    }

    override def |(other: Grammar[TToken, TokenCategory, T]): Grammar[TToken, TokenCategory, T] =
      other match {
        case other: UnionGrammar[TToken, TokenCategory, T] =>
          UnionGrammar(grammars.append(other.grammars))

        case _ =>
          UnionGrammar((other <:: grammars.reverse).reverse)
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

}
