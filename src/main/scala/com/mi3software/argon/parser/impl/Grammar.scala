package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser.GrammarError
import com.mi3software.argon.util._
import org.apache.commons.lang3.StringEscapeUtils

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

  def compact(pos: FilePosition): Grammar[TToken, TSyntaxError, T] = compactImpl(pos, Set.empty)
  protected def compactImpl(pos: FilePosition, seen: Set[Grammar[TToken, TSyntaxError, _]]): Grammar[TToken, TSyntaxError, T]

  protected lazy val isReject: Boolean = isRejectImpl(Set.empty)
  protected def isRejectImpl(seen: Set[Grammar[TToken, TSyntaxError, _]]): Boolean

  protected lazy val isEmptyStr: Boolean = isEmptyStrImpl(Set.empty)
  protected def isEmptyStrImpl(seen: Set[Grammar[TToken, TSyntaxError, _]]): Boolean


  final def sequenceHandler: SequenceHandler[WithSource[TToken], FilePosition, TErrorList \/ NonEmptyList[WithSource[T]]] =
    new SequenceHandler[WithSource[TToken], FilePosition, TErrorList \/ NonEmptyList[WithSource[T]]] {

      override type TState = Grammar[TToken, TSyntaxError, T]

      override def initialState: TState = Grammar.this

      override def next(item: WithSource[TToken], state: TState): TState =
        state.derive(item).compact(item.location.end)

      override def end(terminator: FilePosition, state: Grammar[TToken, TSyntaxError, T]): TErrorList \/ NonEmptyList[WithSource[T]] =
        state.endOfInput(terminator)
    }


  protected def mapNonLazy[U](f: WithSource[T] => WithSource[U]): Grammar[TToken, TSyntaxError, U]

  protected def toStringImpl(seen: Set[Grammar[TToken, TSyntaxError, _]]): String
  override def toString: String = toStringImpl(Set.empty)
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

      private[Grammar] def flatMap[U](f: WithSource[T] => Grammar[TToken, TSyntaxError, U]): Grammar[TToken, TSyntaxError, U] =
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
        this ++ (this*) --> { case (head, tail) => NonEmptyList.nel(head, tail.toIList) }

      def *
      (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
      : Grammar[TToken, TSyntaxError, Vector[T]] =
        new RepeatGrammar(grammar1)

      def observeSource: Grammar[TToken, TSyntaxError, WithSource[T]] = -+> {
        case value @ WithSource(_, location) => WithSource(value, location)
      }


      def streamInto[U, V]
      (grammar2: Grammar[U, TSyntaxError, V])
      (f: PartialFunction[T, U])
      (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
      : Grammar[TToken, TSyntaxError, V] =
        EmptyStrGrammar(NonEmptyList(WithSource((), SourceLocation.empty))).flatMapPos[V] { (_, pos) =>
          grammar2.endOfInput(pos) match {
            case -\/(error) => RejectGrammar(error)
            case \/-(result) => EmptyStrGrammar(result)
          }
        } |
          flatMap { case WithSource(item, location) =>
            f.lift(item) match {
              case Some(u) => streamInto(grammar2.derive(WithSource(u, location)))(f)
              case None => streamInto(grammar2)(f)
            }
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

  private def fromResult[TToken, TSyntaxError, TTokenCategory, T]
  (result: NonEmptyList[TSyntaxError] \/ NonEmptyList[WithSource[T]])
  (implicit errorFactory: ErrorFactory[TToken, TTokenCategory, TSyntaxError])
  : Grammar[TToken, TSyntaxError, T] =
    result match {
      case -\/(errorList) => RejectGrammar(errorList)
      case \/-(results) => EmptyStrGrammar(results)
    }

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

    override protected def endOfInputImpl(pos: FilePosition, seen: Set[Grammar[TToken, TSyntaxError, _]]): TErrorList \/ NonEmptyList[WithSource[T]] = -\/(grammarErrors)

    override def compact(pos: FilePosition): Grammar[TToken, TSyntaxError, T] = this
    override protected def compactImpl(pos: FilePosition, seen: Set[Grammar[TToken, TSyntaxError, _]]): Grammar[TToken, TSyntaxError, T] = this

    override protected def isRejectImpl(seen: Set[Grammar[TToken, TSyntaxError, _]]): Boolean = true
    override protected def isEmptyStrImpl(seen: Set[Grammar[TToken, TSyntaxError, _]]): Boolean = false


    override protected def mapNonLazy[U](f: WithSource[T] => WithSource[U]): Grammar[TToken, TSyntaxError, U] = changeType

    def changeType[U]: RejectGrammar[TToken, TSyntaxError, U] = RejectGrammar(grammarErrors)

    override protected def toStringImpl(seen: Set[Grammar[TToken, TSyntaxError, _]]): String =
      s"Reject ${grammarErrors.list}"
  }

  private final case class EmptyStrGrammar[TToken, TSyntaxError, T]
  (result: NonEmptyList[WithSource[T]])
  (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
    extends Grammar[TToken, TSyntaxError, T] {

    override def derive(token: WithSource[TToken]): RejectGrammar[TToken, TSyntaxError, T] =
      RejectGrammar(NonEmptyList(errorFactory.createError(GrammarError.ExpectedEndOfFile(token))))

    override def endOfInputImpl(pos: FilePosition, seen: Set[Grammar[TToken, TSyntaxError, _]]): TErrorList \/ NonEmptyList[WithSource[T]] = \/-(result)

    override def compact(pos: FilePosition): Grammar[TToken, TSyntaxError, T] = this
    override protected def compactImpl(pos: FilePosition, seen: Set[Grammar[TToken, TSyntaxError, _]]): Grammar[TToken, TSyntaxError, T] = this

    override protected def isRejectImpl(seen: Set[Grammar[TToken, TSyntaxError, _]]): Boolean = false
    override protected def isEmptyStrImpl(seen: Set[Grammar[TToken, TSyntaxError, _]]): Boolean = true

    override protected def mapNonLazy[U](f: WithSource[T] => WithSource[U]): Grammar[TToken, TSyntaxError, U] =
      EmptyStrGrammar(result.map(f))

    def flatMapPos[U](f: (WithSource[T], FilePosition) => Grammar[TToken, TSyntaxError, U]): Grammar[TToken, TSyntaxError, U] =
      new FlatMapPosGrammar(this)(f)

    override protected def toStringImpl(seen: Set[Grammar[TToken, TSyntaxError, _]]): String =
      s"EmptyStr ${result.list}"

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

    override def compact(pos: FilePosition): Grammar[TToken, TSyntaxError, T] = this
    override protected def compactImpl(pos: FilePosition, seen: Set[Grammar[TToken, TSyntaxError, _]]): Grammar[TToken, TSyntaxError, T] = this

    override protected def isRejectImpl(seen: Set[Grammar[TToken, TSyntaxError, _]]): Boolean = false
    override protected def isEmptyStrImpl(seen: Set[Grammar[TToken, TSyntaxError, _]]): Boolean = false

    override protected def mapNonLazy[U](f: WithSource[T] => WithSource[U]): Grammar[TToken, TSyntaxError, U] =
      this -+> f

    override protected def toStringImpl(seen: Set[Grammar[TToken, TSyntaxError, _]]): String =
      StringEscapeUtils.escapeJava(s"Token $category")
  }

  private final class ConcatGrammar[TToken, TSyntaxError, A, B, T]
  (
    grammarAUncached: => Grammar[TToken, TSyntaxError, A],
    grammarBUncached: => Grammar[TToken, TSyntaxError, B],
    combine: (WithSource[A], WithSource[B]) => WithSource[T]
  )(implicit
    errorFactory: ErrorFactory[TToken, _, TSyntaxError]
  ) extends CachedGrammar[TToken, TSyntaxError, T] {

    private lazy val grammarA = grammarAUncached
    private lazy val grammarB = grammarBUncached

    override protected def deriveImpl(token: WithSource[TToken]): Grammar[TToken, TSyntaxError, T] =
      grammarA.endOfInput(token.location.start) match {
        case -\/(_) => ConcatGrammar(grammarA.derive(token), grammarB)(combine)
        case \/-(aValues) =>
          UnionGrammar(
            UnionGrammar.fromList(aValues.map { itemA =>
              Lazy(grammarB.derive(token) -+> { itemB => combine(itemA, itemB) })
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


    override protected def compactImpl(pos: FilePosition, seen: Set[Grammar[TToken, TSyntaxError, _]]): Grammar[TToken, TSyntaxError, T] =
      if(seen contains this)
        this
      else if(isReject || isEmptyStr)
        fromResult(endOfInput(pos))
      else if(grammarA.isEmptyStr) {
        grammarA.endOfInput(pos) match {
          case -\/(errorList) => RejectGrammar(errorList)
          case \/-(aItems) =>
            UnionGrammar.fromList(
              aItems.map { a => Lazy { grammarB -+> { b => combine(a, b) } } }
            )
        }
      }
      else if(grammarB.isEmptyStr) {
        grammarB.endOfInput(pos) match {
          case -\/(errorList) => RejectGrammar(errorList)
          case \/-(bItems) =>
            UnionGrammar.fromList(
              bItems.map { b => Lazy { grammarA -+> { a => combine(a, b) } } }
            )
        }
      }
      else {
        val gAc = grammarA.compactImpl(pos, seen + this)
        val gBc = grammarB.compactImpl(pos, seen + this)

        implicit val equalInstA = ReferenceEqual[Grammar[TToken, TSyntaxError, A]]
        implicit val equalInstB = ReferenceEqual[Grammar[TToken, TSyntaxError, B]]

        if((gAc =/= grammarA) || (gBc =/= grammarB))
          ConcatGrammar(gAc, gBc)(combine)
        else
          this
      }

    override protected def isRejectImpl(seen: Set[Grammar[TToken, TSyntaxError, _]]): Boolean =
      if(seen contains this)
        false
      else
        grammarA.isRejectImpl(seen + this) || grammarB.isRejectImpl(seen + this)

    override protected def isEmptyStrImpl(seen: Set[Grammar[TToken, TSyntaxError, _]]): Boolean =
      if(seen contains this)
        false
      else
        grammarA.isRejectImpl(seen + this) && grammarB.isRejectImpl(seen + this)

    override protected def mapNonLazy[U](f: WithSource[T] => WithSource[U]): Grammar[TToken, TSyntaxError, U] =
      ConcatGrammar(grammarA, grammarB)((a, b) => f(combine(a, b)))

    override protected def toStringImpl(seen: Set[Grammar[TToken, TSyntaxError, _]]): String =
      if(seen contains this)
        "recursive rule"
      else
        s"(${grammarA.toStringImpl(seen + this)} ++ ${grammarB.toStringImpl(seen + this)})"
  }

  private object ConcatGrammar {
    def apply[TToken, TSyntaxError, A, B, T]
    (
      grammarA: => Grammar[TToken, TSyntaxError, A],
      grammarB: => Grammar[TToken, TSyntaxError, B]
    )(
      combine: (WithSource[A], WithSource[B]) => WithSource[T]
    )(implicit
      errorFactory: ErrorFactory[TToken, _, TSyntaxError]
    ): ConcatGrammar[TToken, TSyntaxError, A, B, T] =
      new ConcatGrammar(grammarA, grammarB, combine)

  }

  private final class UnionGrammar[TToken, TSyntaxError, T]
  (grammarAUncached: => Grammar[TToken, TSyntaxError, T], grammarBUncached: => Grammar[TToken, TSyntaxError, T])
  (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
    extends Grammar[TToken, TSyntaxError, T] {

    private lazy val grammarA = grammarAUncached
    private lazy val grammarB = grammarBUncached

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

    override protected def compactImpl(pos: FilePosition, seen: Set[Grammar[TToken, TSyntaxError, _]]): Grammar[TToken, TSyntaxError, T] =
      if(seen contains this)
        this
      else if(isReject || isEmptyStr)
        fromResult(endOfInput(pos))
      else if(grammarA.isReject)
        grammarB.compactImpl(pos, seen + this)
      else if(grammarB.isReject)
        grammarA.compactImpl(pos, seen + this)
      else {
        val gAc = grammarA.compactImpl(pos, seen + this)
        val gBc = grammarB.compactImpl(pos, seen + this)

        implicit val equalInst = ReferenceEqual[Grammar[TToken, TSyntaxError, T]]

        if((gAc =/= grammarA) || (gBc =/= grammarB))
          UnionGrammar(gAc, gBc)
        else
          this
      }


    override protected def isRejectImpl(seen: Set[Grammar[TToken, TSyntaxError, _]]): Boolean =
      if(seen contains this)
        false
      else
        grammarA.isRejectImpl(seen + this) && grammarB.isRejectImpl(seen + this)

    override protected def isEmptyStrImpl(seen: Set[Grammar[TToken, TSyntaxError, _]]): Boolean =
      if(seen contains this)
        false
      else
        grammarA.isEmptyStrImpl(seen + this) && grammarB.isEmptyStrImpl(seen + this)

    override protected def mapNonLazy[U](f: WithSource[T] => WithSource[U]): Grammar[TToken, TSyntaxError, U] =
      UnionGrammar(grammarA.mapNonLazy(f), grammarB.mapNonLazy(f))

    override protected def toStringImpl(seen: Set[Grammar[TToken, TSyntaxError, _]]): String =
      if(seen contains this)
        "recursive rule"
      else
        s"(${grammarA.toStringImpl(seen + this)} | ${grammarB.toStringImpl(seen + this)})"
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

  private final class RepeatGrammar[TToken, TSyntaxError, T]
  (innerUncached: => Grammar[TToken, TSyntaxError, T])
  (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
    extends Grammar[TToken, TSyntaxError, Vector[T]] {

    private lazy val inner = innerUncached

    override def derive(token: WithSource[TToken]): Grammar[TToken, TSyntaxError, Vector[T]] =
      inner.endOfInput(token.location.start) match {
        case -\/(_) => new RepeatBuilderGrammar(inner.derive(token), inner, token.location.start, token.location.end, Vector.empty)
        case \/-(aValues) =>
          UnionGrammar(
            UnionGrammar.fromList(aValues.map { case WithSource(itemA, _) =>
              Lazy(new RepeatBuilderGrammar(inner.derive(token), inner, token.location.start, token.location.end, Vector(itemA)))
            }),
            new RepeatBuilderGrammar(inner.derive(token), inner, token.location.start, token.location.end, Vector.empty)
          )
      }

    override protected def endOfInputImpl(pos: FilePosition, seen: Set[Grammar[TToken, TSyntaxError, _]]): TErrorList \/ NonEmptyList[WithSource[Vector[T]]] =
      \/-(NonEmptyList(WithSource(Vector.empty, SourceLocation(pos, pos))))

    override protected def compactImpl(pos: FilePosition, seen: Set[Grammar[TToken, TSyntaxError, _]]): Grammar[TToken, TSyntaxError, Vector[T]] =
      if(inner.isReject)
        fromResult(endOfInput(pos))
      else
        this

    override protected def isRejectImpl(seen: Set[Grammar[TToken, TSyntaxError, _]]): Boolean =
      if(seen contains this)
        false
      else
        inner.isRejectImpl(seen + this)


    override protected def isEmptyStrImpl(seen: Set[Grammar[TToken, TSyntaxError, _]]): Boolean =
      if(seen contains this)
        false
      else
        inner.isEmptyStrImpl(seen + this)

    override protected def mapNonLazy[U](f: WithSource[Vector[T]] => WithSource[U]): Grammar[TToken, TSyntaxError, U] =
      this -+> f

    override protected def toStringImpl(seen: Set[Grammar[TToken, TSyntaxError, _]]): String =
      s"Repeat ${inner}"
  }

  private final class RepeatBuilderGrammar[TToken, TSyntaxError, T]
  (
    item: Grammar[TToken, TSyntaxError, T],
    inner: Grammar[TToken, TSyntaxError, T],
    startPos: FilePosition,
    endPos: FilePosition,
    items: Vector[T]
  )
  (implicit errorFactory: ErrorFactory[TToken, _, TSyntaxError])
    extends Grammar[TToken, TSyntaxError, Vector[T]] {

    override def derive(token: WithSource[TToken]): Grammar[TToken, TSyntaxError, Vector[T]] =
      item.endOfInput(token.location.start) match {
        case -\/(_) => new RepeatBuilderGrammar(item.derive(token), inner, startPos, token.location.end, items)
        case \/-(aValues) =>
          UnionGrammar(
            UnionGrammar.fromList(aValues.map { case WithSource(itemA, _) =>
              Lazy(new RepeatBuilderGrammar(inner.derive(token), inner, startPos, token.location.end, items :+ itemA))
            }),
            new RepeatBuilderGrammar(item.derive(token), inner, startPos, token.location.end, items)
          )
      }

    override protected def endOfInputImpl(pos: FilePosition, seen: Set[Grammar[TToken, TSyntaxError, _]]): TErrorList \/ NonEmptyList[WithSource[Vector[T]]] =
      item.endOfInputImpl(pos, seen).map { results =>
        results.map { case WithSource(itemValue, _) => WithSource(items :+ itemValue, SourceLocation(startPos, endPos)) }
      }

    override protected def compactImpl(pos: FilePosition, seen: Set[Grammar[TToken, TSyntaxError, _]]): Grammar[TToken, TSyntaxError, Vector[T]] =
      if(seen contains this)
        this
      else if(item.isReject)
        fromResult(endOfInput(pos))
      else {
        val newItem = item.compactImpl(pos, seen + this)

        implicit val refEq = ReferenceEqual[Grammar[TToken, TSyntaxError, T]]

        if(item =/= newItem)
          new RepeatBuilderGrammar(newItem, inner, startPos, endPos, items)
        else
          this
      }

    override protected def isRejectImpl(seen: Set[Grammar[TToken, TSyntaxError, _]]): Boolean =
      if(seen contains this)
        false
      else
        item.isRejectImpl(seen + this) || inner.isRejectImpl(seen + this)

    override protected def isEmptyStrImpl(seen: Set[Grammar[TToken, TSyntaxError, _]]): Boolean =
      if(seen contains this)
        false
      else
        item.isEmptyStrImpl(seen + this) && inner.isEmptyStrImpl(seen + this)

    override protected def mapNonLazy[U](f: WithSource[Vector[T]] => WithSource[U]): Grammar[TToken, TSyntaxError, U] =
      this -+> f

    override protected def toStringImpl(seen: Set[Grammar[TToken, TSyntaxError, _]]): String =
      s"Building ${item} Repeat ${inner}"
  }

  private final class MapGrammar[TToken, TSyntaxError, T, U](innerUncached: => Grammar[TToken, TSyntaxError, T], f: WithSource[T] => WithSource[U]) extends CachedGrammar[TToken, TSyntaxError, U] {

    private lazy val inner = innerUncached

    override protected def deriveImpl(token: WithSource[TToken]): Grammar[TToken, TSyntaxError, U] =
      inner.derive(token) -+> f

    override protected def endOfInputImpl(pos: FilePosition, seen: Set[Grammar[TToken, TSyntaxError, _]]): TErrorList \/ NonEmptyList[WithSource[U]] =
      inner.endOfInputImpl(pos, seen).map(_.map(f))

    override protected def compactImpl(pos: FilePosition, seen: Set[Grammar[TToken, TSyntaxError, _]]): Grammar[TToken, TSyntaxError, U] =
      inner.compactImpl(pos, seen).mapNonLazy(f)

    override protected def isRejectImpl(seen: Set[Grammar[TToken, TSyntaxError, _]]): Boolean =
      inner.isRejectImpl(seen)

    override protected def isEmptyStrImpl(seen: Set[Grammar[TToken, TSyntaxError, _]]): Boolean =
      inner.isEmptyStrImpl(seen)

    override protected def mapNonLazy[V](g: WithSource[U] => WithSource[V]): Grammar[TToken, TSyntaxError, V] =
      inner.mapNonLazy(f andThen g)

    override protected def toStringImpl(seen: Set[Grammar[TToken, TSyntaxError, _]]): String =
      s"Mapped ${inner.toStringImpl(seen)}"
  }

  private final class FlatMapGrammar[TToken, TSyntaxError, T, U]
  (innerUncached: => Grammar[TToken, TSyntaxError, T])
  (f: WithSource[T] => Grammar[TToken, TSyntaxError, U])
  extends Grammar[TToken, TSyntaxError, U] {

    private lazy val inner = innerUncached

    override def derive(token: WithSource[TToken]): Grammar[TToken, TSyntaxError, U] =
      new FlatMapGrammar(inner.derive(token))(f)

    override protected def endOfInputImpl(pos: FilePosition, seen: Set[Grammar[TToken, TSyntaxError, _]]): TErrorList \/ NonEmptyList[WithSource[U]] =
      inner.endOfInput(pos).flatMap { aItems =>
        aItems
          .traverse[TErrorList \/ ?, NonEmptyList[WithSource[U]]] { a => f(a).endOfInput(pos) }
          .map { _.flatMap(identity) }
      }

    override protected def compactImpl(pos: FilePosition, seen: Set[Grammar[TToken, TSyntaxError, _]]): Grammar[TToken, TSyntaxError, U] =
      new FlatMapGrammar(inner.compactImpl(pos, seen))(f)

    override protected def isRejectImpl(seen: Set[Grammar[TToken, TSyntaxError, _]]): Boolean =
      inner.isRejectImpl(seen)

    override protected def isEmptyStrImpl(seen: Set[Grammar[TToken, TSyntaxError, _]]): Boolean =
      false

    override protected def mapNonLazy[V](g: WithSource[U] => WithSource[V]): Grammar[TToken, TSyntaxError, V] =
      new FlatMapGrammar(inner)(t => f(t).mapNonLazy(g))

    override protected def toStringImpl(seen: Set[Grammar[TToken, TSyntaxError, _]]): String =
      s"FlatMap ${inner.toStringImpl(seen)} -> ?"
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

    override protected def compactImpl(pos: FilePosition, seen: Set[Grammar[TToken, TSyntaxError, _]]): Grammar[TToken, TSyntaxError, U] =
      this

    override protected def isRejectImpl(seen: Set[Grammar[TToken, TSyntaxError, _]]): Boolean =
      false

    override protected def isEmptyStrImpl(seen: Set[Grammar[TToken, TSyntaxError, _]]): Boolean =
      false

    override protected def mapNonLazy[V](g: WithSource[U] => WithSource[V]): Grammar[TToken, TSyntaxError, V] =
      new FlatMapPosGrammar(inner)((t, pos) => f(t, pos).mapNonLazy(g))

    override protected def toStringImpl(seen: Set[Grammar[TToken, TSyntaxError, _]]): String =
      s"FlatMapPos ${(inner : Grammar[TToken, TSyntaxError, T]).toStringImpl(seen)} -> ?"
  }


}
