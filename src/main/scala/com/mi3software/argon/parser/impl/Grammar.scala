package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser.impl.Grammar.GrammarError
import com.mi3software.argon.util.{FilePosition, Lazy, SourceLocation, WithSource}

import scala.collection.immutable._
import scala.language.postfixOps
import scalaz._
import Scalaz._

sealed trait Grammar[TToken, TokenCategory, T] {

  type TokenStream[M[_]] = StreamT[M, WithSource[TToken]]
  type TGrammarError = GrammarError[TToken, TokenCategory]
  type TErrorList = NonEmptyList[TGrammarError]

  def derive(token: WithSource[TToken]): Grammar[TToken, TokenCategory, T]

  final def endOfInput(pos: FilePosition): TErrorList \/ NonEmptyList[WithSource[T]] = endOfInputImpl(pos, Set())
  protected def endOfInputImpl(pos: FilePosition, seen: Set[Grammar[TToken, TokenCategory, _]]): TErrorList \/ NonEmptyList[WithSource[T]]


  def shortCircuit: Boolean = false
  def hasResult: Boolean = false


}

object Grammar {

  trait CombinerBase {

    trait GrammarConcatCombiner[A, B, T] {
      def combine(a: A, b: B): T
    }


    implicit def valueConcatCombiner[A, B]: GrammarConcatCombiner[A, B, (A, B)] = (a, b) => (a, b)
  }

  object Operators extends CombinerBase {

    final implicit class GrammarOperatorsImpl[TToken, TokenCategory, T](grammar1: => Grammar[TToken, TokenCategory, T]) {

      def --> [U](f: T => U): Grammar[TToken, TokenCategory, U] = -+>(WithSource.lift(f))
      def -+> [U](f: WithSource[T] => WithSource[U]): Grammar[TToken, TokenCategory, U] =
        new MapGrammar[TToken, TokenCategory, T, U](grammar1, f)

      def | (grammar2: => Grammar[TToken, TokenCategory, T]): Grammar[TToken, TokenCategory, T] =
        new UnionGrammar[TToken, TokenCategory, T](grammar1, grammar2)

      def ++ [U, V](grammar2: => Grammar[TToken, TokenCategory, U])(implicit combiner: GrammarConcatCombiner[T, U, V]): Grammar[TToken, TokenCategory, V] =
        ConcatGrammar(grammar1, grammar2) { (a, b) => WithSource(combiner.combine(a.value, b.value), SourceLocation.merge(a.location, b.location)) }

      def discard: Grammar[TToken, TokenCategory, Unit] = --> { _ => () }
      def ? : Grammar[TToken, TokenCategory, Option[T]] =
        --> [Option[T]](Some.apply) | EmptyStrGrammar(NonEmptyList(WithSource(None, SourceLocation.empty)))

      def +~ : Grammar[TToken, TokenCategory, NonEmptyList[T]] = this ++ (this*) --> { case (head, tail) => NonEmptyList.nel(head, tail) }
      def * : Grammar[TToken, TokenCategory, IList[T]] = {
        lazy val rep: Grammar[TToken, TokenCategory, IList[T]] = (++(rep)?) --> {
          case Some((head, tail)) => head +: tail
          case None => IList()
        }

        rep
      }

      def observeSource: Grammar[TToken, TokenCategory, WithSource[T]] = -+> {
        case value @ WithSource(_, location) => WithSource(value, location)
      }



    }

    implicit def tuple2ConcatCombiner[A, B, C]: GrammarConcatCombiner[(A, B), C, (A, B, C)] = (t, c) => (t._1, t._2, c)
    implicit def tuple3ConcatCombiner[A, B, C, D]: GrammarConcatCombiner[(A, B, C), D, (A, B, C, D)] = (t, d) => (t._1, t._2, t._3, d)
    implicit def tuple4ConcatCombiner[A, B, C, D, E]: GrammarConcatCombiner[(A, B, C, D), E, (A, B, C, D, E)] = (t, e) => (t._1, t._2, t._3, t._4, e)

  }

  import Operators._

  def token[TToken, TokenCategory](category: TokenCategory, tokenMatches: TToken => Boolean): Grammar[TToken, TokenCategory, TToken] =
    matcher(category, t => Some(t).filter(tokenMatches))

  def tokenSource[TToken, TokenCategory](category: TokenCategory, tokenMatches: WithSource[TToken] => Boolean): Grammar[TToken, TokenCategory, TToken] =
    matcherSource(category, t => Some(t).filter(tokenMatches))

  def matcher[TToken, TokenCategory, Result](category: TokenCategory, tokenMatcher: TToken => Option[Result]): Grammar[TToken, TokenCategory, Result] =
    matcherSource(category, WithSource.liftF(tokenMatcher))

  def matcherSource[TToken, TokenCategory, Result](category: TokenCategory, tokenMatcher: TokenMatcher[TToken, Result]): Grammar[TToken, TokenCategory, Result] =
    TokenGrammar(category, tokenMatcher)

  def partialMatcher[TToken, TokenCategory, Result](category: TokenCategory)(f: PartialFunction[TToken, Result]): Grammar[TToken, TokenCategory, Result] =
    matcher(category, f.lift)


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

  final case class InfiniteRecursion[TToken, TokenCategory](position: FilePosition) extends GrammarError[TToken, TokenCategory] {
    override def location: SourceLocation = SourceLocation(position, FilePosition(position.line, position.position + 1))
  }

  type TokenMatcher[TToken, T] = WithSource[TToken] => Option[WithSource[T]]


  private abstract class CachedGrammar[TToken, TokenCategory, T] extends Grammar[TToken, TokenCategory, T] {

    final override def derive(token: WithSource[TToken]): Grammar[TToken, TokenCategory, T] =
      deriveMemo(token)

    private val deriveMemo = Memo.mutableHashMapMemo(deriveImpl)

    protected def deriveImpl(token: WithSource[TToken]): Grammar[TToken, TokenCategory, T]

  }

  private final case class RejectGrammar[TToken, TokenCategory, T](grammarErrors: NonEmptyList[GrammarError[TToken, TokenCategory]]) extends Grammar[TToken, TokenCategory, T] {

    override def derive(token: WithSource[TToken]): Grammar[TToken, TokenCategory, T] = this


    override protected def endOfInputImpl(pos: FilePosition, seen: Set[Grammar[TToken, TokenCategory, _]]): TErrorList \/ NonEmptyList[WithSource[T]] = -\/(grammarErrors)

    override def shortCircuit: Boolean = true
  }

  private final case class EmptyStrGrammar[TToken, TokenCategory, T](result: NonEmptyList[WithSource[T]]) extends Grammar[TToken, TokenCategory, T] {

    override def derive(token: WithSource[TToken]): Grammar[TToken, TokenCategory, T] =
      RejectGrammar(NonEmptyList(ExpectedEndOfFile(token)))

    override def endOfInputImpl(pos: FilePosition, seen: Set[Grammar[TToken, TokenCategory, _]]): TErrorList \/ NonEmptyList[WithSource[T]] = \/-(result)

    override def hasResult: Boolean = true
  }

  private final case class TokenGrammar[TToken, TokenCategory, T](category: TokenCategory, tokenMatcher: TokenMatcher[TToken, T]) extends CachedGrammar[TToken, TokenCategory, T] {

    override def deriveImpl(token: WithSource[TToken]): Grammar[TToken, TokenCategory, T] =
      tokenMatcher(token) match {
        case Some(res) => EmptyStrGrammar(NonEmptyList(res))
        case None => RejectGrammar(NonEmptyList(UnexpectedToken(category, token)))
      }

    override def endOfInputImpl(pos: FilePosition, seen: Set[Grammar[TToken, TokenCategory, _]]): TErrorList \/ NonEmptyList[WithSource[T]] =
      -\/(NonEmptyList(UnexpectedEndOfFile(category, pos)))

  }

  private final class ConcatGrammar[TToken, TokenCategory, A, B, T]
  (
    grammarA: Grammar[TToken, TokenCategory, A],
    grammarBUncached: => Grammar[TToken, TokenCategory, B],
    combine: (WithSource[A], WithSource[B]) => WithSource[T]
  ) extends CachedGrammar[TToken, TokenCategory, T] {

    private lazy val grammarB = grammarBUncached

    override protected def deriveImpl(token: WithSource[TToken]): Grammar[TToken, TokenCategory, T] =
      grammarA.endOfInput(token.location.start) match {
        case -\/(_) => ConcatGrammar(grammarA.derive(token), grammarB)(combine)
        case \/-(aValues) =>
          UnionGrammar(
            UnionGrammar.fromList(aValues.map { itemA =>
              Lazy(grammarB -+> { itemB => combine(itemA, itemB) })
            }),
            ConcatGrammar(grammarA.derive(token), grammarB)(combine)
          )

      }



    override protected def endOfInputImpl(pos: FilePosition, seen: Set[Grammar[TToken, TokenCategory, _]]): TErrorList \/ NonEmptyList[WithSource[T]] =
      if(seen contains this)
        -\/(NonEmptyList(InfiniteRecursion(pos)))
      else
        for {
          aItems <- grammarA.endOfInputImpl(pos, seen + this)
          bItems <- grammarB.endOfInputImpl(pos, seen + this)
        } yield for {
          a <- aItems
          b <- bItems
        } yield combine(a, b)

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

  }

  private final class UnionGrammar[TToken, TokenCategory, T](grammarA: => Grammar[TToken, TokenCategory, T], grammarB: => Grammar[TToken, TokenCategory, T]) extends Grammar[TToken, TokenCategory, T] {

    override def derive(token: WithSource[TToken]): Grammar[TToken, TokenCategory, T] =
      UnionGrammar(grammarA.derive(token), grammarB.derive(token))

    override protected def endOfInputImpl(pos: FilePosition, seen: Set[Grammar[TToken, TokenCategory, _]]): TErrorList \/ NonEmptyList[WithSource[T]] =
      if(seen contains this)
        -\/(NonEmptyList(InfiniteRecursion(pos)))
      else {
        val seen2 = seen + this

        (grammarA.endOfInputImpl(pos, seen2), grammarB.endOfInputImpl(pos, seen2)) match {
          case (\/-(resultA), \/-(resultB)) => \/-(resultA.append(resultB))
          case (result @ \/-(_), -\/(_)) => result
          case (-\/(_), result @ \/-(_)) => result
          case (-\/(errorA), -\/(errorB)) =>

            def findLastErrorPos(errorList: TErrorList): FilePosition =
              errorList.maximumBy1(_.location.end).location.end

            val cmp = findLastErrorPos(errorA).compareTo(findLastErrorPos(errorB))

            -\/(
              if(cmp > 0)
                errorA
              else if(cmp < 0)
                errorB
              else
                errorA.append(errorB)
            )
        }
      }

  }

  object UnionGrammar {

    def apply[TToken, TokenCategory, T](grammarA: => Grammar[TToken, TokenCategory, T], grammarB: => Grammar[TToken, TokenCategory, T]): Grammar[TToken, TokenCategory, T] =
      new UnionGrammar[TToken, TokenCategory, T](grammarA, grammarB)

    def fromList[TToken, TokenCategory, T](grammars: NonEmptyList[Lazy[Grammar[TToken, TokenCategory, T]]]): Grammar[TToken, TokenCategory, T] =
      grammars.tail match {
        case ICons(head2, tail) =>
          tail.foldLeft(apply(grammars.head.value, head2.value)) { (a, b) => apply(a, b.value) }

        case INil() =>
          grammars.head.value
      }

  }

  private final class MapGrammar[TToken, TokenCategory, T, U](inner: => Grammar[TToken, TokenCategory, T], f: WithSource[T] => WithSource[U]) extends CachedGrammar[TToken, TokenCategory, U] {

    override protected def deriveImpl(token: WithSource[TToken]): Grammar[TToken, TokenCategory, U] =
      inner.derive(token) -+> f

    override protected def endOfInputImpl(pos: FilePosition, seen: Set[Grammar[TToken, TokenCategory, _]]): TErrorList \/ NonEmptyList[WithSource[U]] =
      inner.endOfInputImpl(pos, seen).map(_.map(f))

  }


}
