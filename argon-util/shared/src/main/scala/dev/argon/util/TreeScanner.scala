package dev.argon.util

import scala.compiletime.{erasedValue, summonInline}
import scala.deriving.Mirror
import scala.quoted.{Expr, Quotes, Type}
import cats.*
import cats.implicits.given

trait TreeScanner[F[_]: Monad] {
  type Scanner[T] = TreeScanner.Scanner[F, T]

  object StandardScanners {
    given [A: Scanner]: Scanner[Seq[A]] with
      override def scan(a: Seq[A]): F[Unit] =
        a.traverse_(summon[Scanner[A]].scan)
    end given
    
    given [A: Scanner]: Scanner[Option[A]] with
      override def scan(a: Option[A]): F[Unit] =
        a.traverse_(summon[Scanner[A]].scan)
    end given

    given Scanner[Nothing] with
      override def scan(a: Nothing): F[Unit] = a
    end given
  }

  final class IgnoreScanner[A] extends Scanner[A] {
    override def scan(a: A): F[Unit] = Monad[F].pure(())
  }

  final inline def autoScanner[T <: Matchable](using m: Mirror.Of[T]): Scanner[T] =
    inline m match {
      case m: Mirror.SumOf[T] =>
        autoScannerSum[T](using m)

      case m: Mirror.ProductOf[T] =>
        autoScannerProduct[T](using m)
    }

  final inline def autoScannerSum[T <: Matchable](using m: Mirror.SumOf[T]): Scanner[T] =
    ${ TreeScanner.autoScannerSumMacro[F, T, m.MirroredElemTypes]('this) }

  final inline def autoScannerProduct[T](using m: Mirror.ProductOf[T]): Scanner[T] =
    val tupleComparer = autoComparerTuple[m.MirroredElemTypes]
    val m2 = summonInline[Mirror.ProductOf[T] {type MirroredElemTypes = m.MirroredElemTypes} =:= Mirror.ProductOf[T & Product] {type MirroredElemTypes = m.MirroredElemTypes}](m)

    summonInline[Scanner[T & Product] =:= Scanner[T]](
      ProductScanner[T & Product](using m2)(tupleComparer)
    )
  end autoScannerProduct

  final inline def autoComparerTuple[T <: Tuple]: Scanner[T] =
    inline erasedValue[T] match {
      case _: (th *: tt) =>
        lazy val thComparer = summonInline[Scanner[th]]
        val ttComparer = autoComparerTuple[tt]
        summonInline[Scanner[th *: tt] =:= Scanner[T]](ConsTupleScanner(thComparer, ttComparer))

      case _: EmptyTuple =>
        summonInline[Scanner[EmptyTuple] =:= Scanner[T]](EmptyTupleScanner)
    }

  final class ProductScanner[T <: Product](using m: Mirror.ProductOf[T])(tupleComparer: Scanner[m.MirroredElemTypes]) extends Scanner[T] {
      override def scan(a: T): F[Unit] =
        tupleComparer.scan(Tuple.fromProductTyped(a)(using m))
  }

  final class ConsTupleScanner[TH, TT <: Tuple](thComparer: => Scanner[TH], ttComparer: Scanner[TT]) extends Scanner[TH *: TT] {
    override def scan(a: TH *: TT): F[Unit] =
      val (ah *: at) = a
      thComparer.scan(ah).flatMap { _ => ttComparer.scan(at) }
    end scan
  }
  
  object EmptyTupleScanner extends Scanner[EmptyTuple] {
    override def scan(a: EmptyTuple): F[Unit] =
      Monad[F].pure(())
  }


}

object TreeScanner {
  @FunctionalInterface
  trait Scanner[F[_], T] {
    def scan(a: T): F[Unit]
  }

  def autoScannerSumMacro[F[_]: Type, T <: Matchable : Type, Cases <: Tuple : Type](matcher: Expr[TreeScanner[F]])(using q: Quotes): Expr[Scanner[F, T]] =
    '{
      new Scanner[F, T] {
        override def scan(a: T): F[Unit] =
          ${
            MacroUtils.patternMatch[T, Cases, F[Unit]]('a)([U] => (u: Expr[U], uType: Type[U]) => {
              given Type[U] = uType
              '{ $matcher.autoScannerProduct[U](using summonInline[Mirror.ProductOf[U]]).scan($u) }
            })
          }
      }
    }

}
