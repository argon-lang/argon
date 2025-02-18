package dev.argon.util

import scala.compiletime.{erasedValue, summonInline}
import scala.deriving.Mirror
import scala.quoted.{Expr, Quotes, Type}

trait TreeComparison {
  type Comparison
  def comparisonFromBoolean(b: Boolean): Comparison
  def combineComparison(a: => Comparison, b: => Comparison): Comparison
  def combineAllComparisons[A](a: Seq[A])(f: A => Comparison): Comparison =
    a.foldRight(Lazy(comparisonFromBoolean(true))) { (x, y) => Lazy { combineComparison(f(x), y.value) } }.value

  type Comparer[T] = TreeComparison.Comparer[T, Comparison]

  object StandardComparers {
    given Comparer[Boolean] = EqualComparer[Boolean]
    given Comparer[BigInt] = EqualComparer[BigInt]
    given Comparer[Int] = EqualComparer[Int]
    given Comparer[String] = EqualComparer[String]

    given [A: Comparer] => Comparer[Seq[A]]:
      override def compare(a: Seq[A], b: Seq[A]): Comparison =
        combineComparison(
          comparisonFromBoolean(a.size == b.size),
          combineAllComparisons(a.zip(b))(summon[Comparer[A]].compare),
        )
    end given

    given [A: Comparer] => Comparer[Option[A]]:
      override def compare(a: Option[A], b: Option[A]): Comparison =
        (a, b) match {
          case (Some(a), Some(b)) => summon[Comparer[A]].compare(a, b)
          case (None, None) => comparisonFromBoolean(true)
          case _ => comparisonFromBoolean(false)
        }
    end given
  }

  final class EqualComparer[A](using CanEqual[A, A]) extends Comparer[A] {
    override def compare(a: A, b: A): Comparison =
      comparisonFromBoolean(a == b)
  }


  final inline def autoComparer[T <: Matchable](using m: Mirror.Of[T]): Comparer[T] =
    inline m match {
      case m: Mirror.SumOf[T] =>
        autoComparerSum[T](using m)

      case m: Mirror.ProductOf[T] =>
        autoComparerProduct[T](using m)
    }

  final inline def autoComparerSum[T <: Matchable](using m: Mirror.SumOf[T]): Comparer[T] =
    ${ TreeComparison.autoComparerSumMacro[Comparison, T, m.MirroredElemTypes]('this) }

  final inline def autoComparerProduct[T](using m: Mirror.ProductOf[T]): Comparer[T] =
    val tupleComparer = autoComparerTuple[m.MirroredElemTypes]
    val m2 = summonInline[Mirror.ProductOf[T] {type MirroredElemTypes = m.MirroredElemTypes} =:= Mirror.ProductOf[T & Product] {type MirroredElemTypes = m.MirroredElemTypes}](m)
    summonInline[Comparer[T & Product] =:= Comparer[T]](
      ProductComparer[T & Product](using m2)(tupleComparer)
    )
  end autoComparerProduct

  final inline def autoComparerTuple[T <: Tuple]: Comparer[T] =
    inline erasedValue[T] match {
      case _: (th *: tt) =>
        lazy val thComparer = summonInline[Comparer[th]]
        val ttComparer = autoComparerTuple[tt]
        summonInline[Comparer[th *: tt] =:= Comparer[T]](
          ConsTupleComparer(thComparer, ttComparer)
        )

      case _: EmptyTuple =>
        summonInline[Comparer[EmptyTuple] =:= Comparer[T]](EmptyTupleComparer)
    }

  final class ProductComparer[T <: Product](using m: Mirror.ProductOf[T])(tupleComparer: Comparer[m.MirroredElemTypes]) extends Comparer[T] {
    override def compare(a: T, b: T): Comparison =
      tupleComparer.compare(
        Tuple.fromProductTyped[T](a)(using m),
        Tuple.fromProductTyped[T](b)(using m),
      )
  }

  final class ConsTupleComparer[TH, TT <: Tuple](thComparer: => Comparer[TH], ttComparer: Comparer[TT]) extends Comparer[TH *: TT] {
    override def compare(a: TH *: TT, b: TH *: TT): Comparison =
      val (ah *: at) = a
      val (bh *: bt) = b
      combineComparison(
        thComparer.compare(ah, bh),
        ttComparer.compare(at, bt),
      )
    end compare

  }
  
  object EmptyTupleComparer extends Comparer[EmptyTuple] {
    override def compare(a: EmptyTuple, b: EmptyTuple): Comparison =
      comparisonFromBoolean(true)
  }


}

object TreeComparison {
  type Aux[C] = TreeComparison { type Comparison = C }

  @FunctionalInterface
  trait Comparer[T, C] {
    def compare(a: T, b: T): C
  }

  def autoComparerSumMacro[C: Type, T <: Matchable : Type, Cases <: Tuple : Type](matcher: Expr[TreeComparison.Aux[C]])(using q: Quotes): Expr[Comparer[T, C]] =
    '{
      new Comparer[T, C] {
        override def compare(a: T, b: T): C =
          ${
            MacroUtils.patternMatch2[T, Cases, C]('a, 'b)('{ $matcher.comparisonFromBoolean(false) })([U] => (u1: Expr[U], u2: Expr[U], uType: Type[U]) => {
              given Type[U] = uType
              '{ $matcher.autoComparerProduct[U](using summonInline[Mirror.ProductOf[U]]).compare($u1, $u2) }
            })
          }
      }
    }



}
