package dev.argon.util

import scala.compiletime.ops.int.*
import scala.quoted.*

sealed trait NList[N <: Int, +A] derives CanEqual:
  def +:[B >: A](value: B): NList[S[N], B] = NCons(value, this)

  def asCons[P <: Int](using N =:= S[P]): NCons[P, A] =
    this.asInstanceOf[NCons[P, A]]

  def zip[B](other: NList[N, B]): NList[N, (A, B)]
  def map[B](f: A => B): NList[N, B]

  def iterator: Iterator[A] =
    new Iterator[A] {

      @SuppressWarnings(Array("scalafix:DisableSyntax.var"))
      private var list: NList[?, A] = NList.this

      override def hasNext: Boolean =
        list match {
          case _: NNil.type => false
          case NCons(_, _) => true
        }

      override def next(): A =
        list match {
          case _: NNil.type => throw new NoSuchElementException()
          case NCons(head, tail) =>
            list = tail
            head
        }

    }

  def toIterable: Iterable[A] =
    new Iterable[A] {
      override def iterator: Iterator[A] = NList.this.iterator
    }

  def toSeq: Seq[A] = toIterable.toSeq
  def toVector: Vector[A] = toIterable.toVector

  def makeFactory: NListFactory[N]
end NList

case object NNil extends NList[0, Nothing] {
  override def zip[B](other: NList[0, B]): NList[0, (Nothing, B)] = NNil
  override def map[B](f: Nothing => B): NList[0, B] = NNil

  override def makeFactory: NListFactory[0] = summon[NListFactory[0]]
}

final case class NCons[P <: Int, +A](head: A, tail: NList[P, A]) extends NList[S[P], A] {
  override def zip[B](other: NList[S[P], B]): NList[S[P], (A, B)] = NCons((head, other.head), tail.zip(other.tail))
  override def map[B](f: A => B): NList[S[P], B] = NCons(f(head), tail.map(f))

  override def makeFactory: NListFactory[S[P]] = nListFactorySucc(using tail.makeFactory)
}

object NList {

  def from[A](s: Seq[A]): NList[?, A] =
    s match {
      case h +: t => NCons(h, from(t))
      case _ => NNil
    }

  private def sizedImpl[N <: Int, A](n: Int)(s: Iterator[A]): Option[NList[?, A]] =
    if n > 0 then {
      if s.hasNext then {
        val value = s.next()
        sizedImpl(n - 1)(s).map { t => value +: t }
      }
      else {
        None
      }
    }
    else if s.hasNext then None
    else Some(NNil)

  implicit class NListOps[N <: Int, A](private val list: NList[N, A]) extends AnyVal {
    def head[P <: Int](implicit lenIsSucc: N =:= S[P]): A = list.asCons.head
    def tail[P <: Int](implicit lenIsSucc: N =:= S[P]): NList[P, A] = list.asCons.tail
  }

}

sealed trait NListFactory[N <: Int] {
  def fromSeqPrefix[A](s: Seq[A]): Option[(NList[N, A], Seq[A])]

  final def fromSeq[A](s: Seq[A]): Option[NList[N, A]] =
    fromSeqPrefix(s).flatMap {
      case (nlist, Seq()) => Some(nlist)
      case _ => None
    }

}

given NListFactory[0] with
  override def fromSeqPrefix[A](s: Seq[A]): Option[(NList[0, A], Seq[A])] = Some(NNil, s)
end given

given nListFactorySucc[P <: Int : NListFactory]: NListFactory[S[P]] with {

  override def fromSeqPrefix[A](s: Seq[A]): Option[(NList[S[P], A], Seq[A])] =
    s match {
      case h +: t =>
        summon[NListFactory[P]].fromSeqPrefix(t).map { case (tSized, rest) =>
          (NCons(h, tSized), rest)
        }
      case _ => None
    }

}

given [N <: Int]: Traverse[[A] =>> NList[N, A]] with

  override def traverse[F[+_]: Applicative, A, B](ca: NList[N, A])(f: A => F[B]): F[NList[N, B]] =
    ca match {
      case NCons(h, t) =>
        def consImpl[Prev <: Int](h: A, t: NList[Prev, A]): F[NList[S[Prev], B]] =
          Applicative[F].map2(f(h), Traverse[[X] =>> NList[Prev, X]].traverse(t)(f)) { (h2, t2) => h2 +: t2 }

        consImpl(h, t)

      case ca: NNil.type =>
        Applicative[F].pure(NNil)
    }

  def foldLeftM[F[+_]: Monad, S, A](ca: NList[N, A])(s: S)(f: (S, A) => F[S]): F[S] =
    ca match {
      case NCons(h, t) =>
        def consImpl[Prev <: Int](h: A, t: NList[Prev, A]): F[S] =
          f(s, h).flatMap { s2 => Traverse[[X] =>> NList[Prev, X]].foldLeftM(t)(s2)(f) }

        consImpl(h, t)

      case ca: NNil.type => Applicative[F].pure(s)
    }

  def foldLeft[S, A](ca: NList[N, A])(s: S)(f: (S, A) => S): S = ca.toSeq.foldLeft(s)(f)

  override def map[A, B](fa: NList[N, A])(f: A => B): NList[N, B] = fa.map(f)
end given
