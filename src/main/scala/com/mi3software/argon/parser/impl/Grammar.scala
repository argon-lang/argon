package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser.GrammarError
import com.mi3software.argon.util.{FilePosition, Lazy, SourceLocation, WithSource}

import scala.collection.immutable._
import scala.language.postfixOps
import scalaz._
import Scalaz._

sealed trait Grammar[TToken, TSyntaxError, T] {

  type TokenStream[M[_]] = StreamT[M, WithSource[TToken]]
  type TErrorList = NonEmptyList[TSyntaxError]

  def derive(token: WithSource[TToken]): Grammar[TToken, TSyntaxError, T]

  final def endOfInput(pos: FilePosition): TErrorList \/ NonEmptyList[WithSource[T]] = endOfInputImpl(pos, Set())
  protected def endOfInputImpl(pos: FilePosition, seen: Set[Grammar[TToken, TSyntaxError, _]]): TErrorList \/ NonEmptyList[WithSource[T]]

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

    final implicit class GrammarOperatorsImpl[TToken, TSyntaxError, T](grammar1: => Grammar[TToken, TSyntaxError, T]) {

      def --> [U](f: T => U): Grammar[TToken, TSyntaxError, U] = -+>(WithSource.lift(f))
      def -+> [U](f: WithSource[T] => WithSource[U]): Grammar[TToken, TSyntaxError, U] =
        new MapGrammar[TToken, TSyntaxError, T, U](grammar1, f)

      def flatMap[U](f: WithSource[T] => Grammar[TToken, TSyntaxError, U]): Grammar[TToken, TSyntaxError, U] =
        new FlatMapGrammar(grammar1)(f)

      def |
      (grammar2: => Grammar[TToken, TSyntaxError, T])
      (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
      : Grammar[TToken, TSyntaxError, T] =
        new UnionGrammar[TToken, TSyntaxError, T](grammar1, grammar2)

      def ++ [U, V]
      (grammar2: => Grammar[TToken, TSyntaxError, U])
      (implicit combiner: GrammarConcatCombiner[T, U, V], errorFactory: ErrorFactory[TToken, _, TSyntaxError])
      : Grammar[TToken, TSyntaxError, V] =
        ConcatGrammar(grammar1, grammar2) { (a, b) => WithSource(combiner.combine(a.value, b.value), SourceLocation.merge(a.location, b.location)) }

      def discard: Grammar[TToken, TSyntaxError, Unit] = --> { _ => () }
      def ? (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError]): Grammar[TToken, TSyntaxError, Option[T]] =
        --> [Option[T]](Some.apply) | EmptyStrGrammar(NonEmptyList(WithSource(None, SourceLocation.empty)))

      def +~
      (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
      : Grammar[TToken, TSyntaxError, NonEmptyList[T]] =
        this ++ (this*) --> { case (head, tail) => NonEmptyList.nel(head, tail) }

      def *
      (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
      : Grammar[TToken, TSyntaxError, IList[T]] = {
        lazy val rep: Grammar[TToken, TSyntaxError, IList[T]] = (++(rep)?) --> {
          case Some((head, tail)) => head +: tail
          case None => IList()
        }

        rep
      }

      def observeSource: Grammar[TToken, TSyntaxError, WithSource[T]] = -+> {
        case value @ WithSource(_, location) => WithSource(value, location)
      }


      def streamInto[U]
      (grammar2: Grammar[T, TSyntaxError, U])
      (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
      : Grammar[TToken, TSyntaxError, U] =
        EmptyStrGrammar(NonEmptyList(WithSource((), SourceLocation.empty))).flatMapPos[U] { (_, pos) =>
          grammar2.endOfInput(pos) match {
            case -\/(error) => RejectGrammar(error)
            case \/-(result) => EmptyStrGrammar(result)
          }
        } |
          flatMap { item => streamInto(grammar2.derive(item)) }



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

  def token[TToken, TSyntaxError, TTokenCategory]
  (category: TTokenCategory, tokenMatches: TToken => Boolean)
  (implicit errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError])
  : Grammar[TToken, TSyntaxError, TToken] =
    matcher(category, t => Some(t).filter(tokenMatches))

  def tokenSource[TToken, TSyntaxError, TTokenCategory]
  (category: TTokenCategory, tokenMatches: WithSource[TToken] => Boolean)
  (implicit errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError])
  : Grammar[TToken, TSyntaxError, TToken] =
    matcherSource(category, t => Some(t).filter(tokenMatches))

  def matcher[TToken, TSyntaxError, TTokenCategory, Result]
  (category: TTokenCategory, tokenMatcher: TToken => Option[Result])
  (implicit errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError])
  : Grammar[TToken, TSyntaxError, Result] =
    matcherSource(category, WithSource.liftF(tokenMatcher))

  def matcherSource[TToken, TSyntaxError, TTokenCategory, Result]
  (category: TTokenCategory, tokenMatcher: TokenMatcher[TToken, Result])
  (implicit errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError])
  : Grammar[TToken, TSyntaxError, Result] =
    TokenGrammar(category, tokenMatcher)

  def partialMatcher[TToken, TSyntaxError, TTokenCategory, Result]
  (category: TTokenCategory)
  (f: PartialFunction[TToken, Result])
  (implicit errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError])
  : Grammar[TToken, TSyntaxError, Result] =
    matcher(category, f.lift)

  trait ErrorFactory[-TToken, -TTokenCategory, TSyntaxError] {
    def createError(error: GrammarError[TToken, TTokenCategory]): TSyntaxError
    def errorEndLocationOrder: Order[TSyntaxError]
  }

  type TokenMatcher[TToken, T] = WithSource[TToken] => Option[WithSource[T]]


  private abstract class CachedGrammar[TToken, TSyntaxError, T] extends Grammar[TToken, TSyntaxError, T] {

    final override def derive(token: WithSource[TToken]): Grammar[TToken, TSyntaxError, T] =
      deriveMemo(token)

    private val deriveMemo = Memo.mutableHashMapMemo(deriveImpl)

    protected def deriveImpl(token: WithSource[TToken]): Grammar[TToken, TSyntaxError, T]

  }

  private final case class RejectGrammar[TToken, TSyntaxError, T](grammarErrors: NonEmptyList[TSyntaxError]) extends Grammar[TToken, TSyntaxError, T] {

    override def derive(token: WithSource[TToken]): Grammar[TToken, TSyntaxError, T] = this

    def changeType[U]: RejectGrammar[TToken, TSyntaxError, U] = RejectGrammar(grammarErrors)


    override protected def endOfInputImpl(pos: FilePosition, seen: Set[Grammar[TToken, TSyntaxError, _]]): TErrorList \/ NonEmptyList[WithSource[T]] = -\/(grammarErrors)

    override def shortCircuit: Boolean = true
  }

  private final case class EmptyStrGrammar[TToken, TSyntaxError, T]
  (result: NonEmptyList[WithSource[T]])
  (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
    extends Grammar[TToken, TSyntaxError, T] {

    override def derive(token: WithSource[TToken]): RejectGrammar[TToken, TSyntaxError, T] =
      RejectGrammar(NonEmptyList(errorFactory.createError(GrammarError.ExpectedEndOfFile(token))))

    override def endOfInputImpl(pos: FilePosition, seen: Set[Grammar[TToken, TSyntaxError, _]]): TErrorList \/ NonEmptyList[WithSource[T]] = \/-(result)

    def flatMapPos[U](f: (WithSource[T], FilePosition) => Grammar[TToken, TSyntaxError, U]): Grammar[TToken, TSyntaxError, U] =
      new FlatMapPosGrammar(this)(f)

    override def hasResult: Boolean = true
  }

  private final case class TokenGrammar[TToken, TSyntaxError, TTokenCategory, T]
  (
    category: TTokenCategory,
    tokenMatcher: TokenMatcher[TToken, T]
  )(implicit
    errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError]
  ) extends CachedGrammar[TToken, TSyntaxError, T] {

    override def deriveImpl(token: WithSource[TToken]): Grammar[TToken, TSyntaxError, T] =
      tokenMatcher(token) match {
        case Some(res) => EmptyStrGrammar(NonEmptyList(res))
        case None => RejectGrammar(NonEmptyList(errorFactory.createError(GrammarError.UnexpectedToken(category, token))))
      }

    override def endOfInputImpl(pos: FilePosition, seen: Set[Grammar[TToken, TSyntaxError, _]]): TErrorList \/ NonEmptyList[WithSource[T]] =
      -\/(NonEmptyList(errorFactory.createError(GrammarError.UnexpectedEndOfFile(category, pos))))

  }

  private final class ConcatGrammar[TToken, TSyntaxError, A, B, T]
  (
    grammarA: Grammar[TToken, TSyntaxError, A],
    grammarBUncached: => Grammar[TToken, TSyntaxError, B],
    combine: (WithSource[A], WithSource[B]) => WithSource[T]
  )(implicit
    errorFactory: ErrorFactory[TToken, _, TSyntaxError]
  ) extends CachedGrammar[TToken, TSyntaxError, T] {

    private lazy val grammarB = grammarBUncached

    override protected def deriveImpl(token: WithSource[TToken]): Grammar[TToken, TSyntaxError, T] =
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



    override protected def endOfInputImpl(pos: FilePosition, seen: Set[Grammar[TToken, TSyntaxError, _]]): TErrorList \/ NonEmptyList[WithSource[T]] =
      if(seen contains this)
        -\/(NonEmptyList(errorFactory.createError(GrammarError.InfiniteRecursion(pos))))
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
    def apply[TToken, TSyntaxError, A, B, T]
    (
      grammarA: Grammar[TToken, TSyntaxError, A],
      grammarB: => Grammar[TToken, TSyntaxError, B]
    )(
      combine: (WithSource[A], WithSource[B]) => WithSource[T]
    )(implicit
      errorFactory: ErrorFactory[TToken, _, TSyntaxError]
    ): ConcatGrammar[TToken, TSyntaxError, A, B, T] =
      new ConcatGrammar(grammarA, grammarB, combine)

  }

  private final class UnionGrammar[TToken, TSyntaxError, T]
  (grammarA: => Grammar[TToken, TSyntaxError, T], grammarB: => Grammar[TToken, TSyntaxError, T])
  (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
    extends Grammar[TToken, TSyntaxError, T] {

    override def derive(token: WithSource[TToken]): Grammar[TToken, TSyntaxError, T] =
      UnionGrammar(grammarA.derive(token), grammarB.derive(token))

    override protected def endOfInputImpl(pos: FilePosition, seen: Set[Grammar[TToken, TSyntaxError, _]]): TErrorList \/ NonEmptyList[WithSource[T]] =
      if(seen contains this)
        -\/(NonEmptyList(errorFactory.createError(GrammarError.InfiniteRecursion(pos))))
      else {
        val seen2 = seen + this

        (grammarA.endOfInputImpl(pos, seen2), grammarB.endOfInputImpl(pos, seen2)) match {
          case (\/-(resultA), \/-(resultB)) => \/-(resultA.append(resultB))
          case (result @ \/-(_), -\/(_)) => result
          case (-\/(_), result @ \/-(_)) => result
          case (-\/(errorA), -\/(errorB)) =>

            def findLastErrorPos(errorList: TErrorList): TSyntaxError =
              errorList.maximum1(errorFactory.errorEndLocationOrder)

            -\/(
              errorFactory.errorEndLocationOrder(findLastErrorPos(errorA), findLastErrorPos(errorB)) match {
                case Ordering.GT => errorA
                case Ordering.LT => errorB
                case Ordering.EQ => errorA.append(errorB)
              }
            )
        }
      }

  }

  object UnionGrammar {

    def apply[TToken, TSyntaxError, T]
    (grammarA: => Grammar[TToken, TSyntaxError, T], grammarB: => Grammar[TToken, TSyntaxError, T])
    (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
    : Grammar[TToken, TSyntaxError, T] =
      new UnionGrammar[TToken, TSyntaxError, T](grammarA, grammarB)

    def fromList[TToken, TSyntaxError, T]
    (grammars: NonEmptyList[Lazy[Grammar[TToken, TSyntaxError, T]]])
    (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
    : Grammar[TToken, TSyntaxError, T] =
      grammars.tail match {
        case ICons(head2, tail) =>
          tail.foldLeft(apply(grammars.head.value, head2.value)) { (a, b) => apply(a, b.value) }

        case INil() =>
          grammars.head.value
      }

  }

  private final class MapGrammar[TToken, TSyntaxError, T, U](inner: => Grammar[TToken, TSyntaxError, T], f: WithSource[T] => WithSource[U]) extends CachedGrammar[TToken, TSyntaxError, U] {

    override protected def deriveImpl(token: WithSource[TToken]): Grammar[TToken, TSyntaxError, U] =
      inner.derive(token) -+> f

    override protected def endOfInputImpl(pos: FilePosition, seen: Set[Grammar[TToken, TSyntaxError, _]]): TErrorList \/ NonEmptyList[WithSource[U]] =
      inner.endOfInputImpl(pos, seen).map(_.map(f))

  }

  private final class FlatMapGrammar[TToken, TSyntaxError, T, U]
  (inner: Grammar[TToken, TSyntaxError, T])
  (f: WithSource[T] => Grammar[TToken, TSyntaxError, U])
  extends Grammar[TToken, TSyntaxError, U] {

    override def derive(token: WithSource[TToken]): Grammar[TToken, TSyntaxError, U] =
      new FlatMapGrammar(inner.derive(token))(f)

    override protected def endOfInputImpl(pos: FilePosition, seen: Set[Grammar[TToken, TSyntaxError, _]]): TErrorList \/ NonEmptyList[WithSource[U]] =
      inner.endOfInput(pos).flatMap { aItems =>
        aItems
          .traverse[TErrorList \/ ?, NonEmptyList[WithSource[U]]] { a => f(a).endOfInput(pos) }
          .map { _.flatMap(identity) }
      }

  }

  private final class FlatMapPosGrammar[TToken, TSyntaxError, T, U]
  (inner: EmptyStrGrammar[TToken, TSyntaxError, T])
  (f: (WithSource[T], FilePosition) => Grammar[TToken, TSyntaxError, U])
    extends Grammar[TToken, TSyntaxError, U] {

    override def derive(token: WithSource[TToken]): Grammar[TToken, TSyntaxError, U] =
      inner.derive(token).changeType[U]

    override protected def endOfInputImpl(pos: FilePosition, seen: Set[Grammar[TToken, TSyntaxError, _]]): TErrorList \/ NonEmptyList[WithSource[U]] =
      inner.endOfInput(pos).flatMap { aItems =>
        aItems
          .traverse[TErrorList \/ ?, NonEmptyList[WithSource[U]]] { a => f(a, pos).endOfInput(pos) }
          .map { _.flatMap(identity) }
      }

  }


}
