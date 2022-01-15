package dev.argon.util

opaque type NonEmptyList[+A] = ::[A]

object NonEmptyList:
  def apply[A](head: A, tail: A*): NonEmptyList[A] = ::.apply(head, tail.toList)

  def cons[A](head: A, tail: NonEmptyList[A] | List[A]): NonEmptyList[A] = ::.apply(head, tail)

  def unapply[A](l: NonEmptyList[A]): Some[(A, List[A])] = Some((l.head, l.tail))

  def fromList[A](l: List[A]): Option[NonEmptyList[A]] =
    l match {
      case l: ::[A] => Some(l)
      case _: Nil.type => None
    }

  def fromCons[A](l: ::[A]): NonEmptyList[A] = l

end NonEmptyList

extension [A](l: NonEmptyList[A])
  def head: A = l.head
  def tail: List[A] = l.tail
  def toList: List[A] = l
  def reverse: NonEmptyList[A] = l.reverse.asInstanceOf[NonEmptyList[A]]
end extension

given TraverseNonEmpty[NonEmptyList] with Monad[NonEmptyList] with

  override def flatMap[A, B](fa: NonEmptyList[A])(f: A => NonEmptyList[B]): NonEmptyList[B] =
    fa.flatMap(f).asInstanceOf[NonEmptyList[B]]

  override def pure[A](a: A): NonEmptyList[A] = NonEmptyList(a)

  override def traverse[F[+_]: Applicative, A, B](ca: NonEmptyList[A])(f: A => F[B]): F[NonEmptyList[B]] =
    Applicative[F].map2(f(ca.head), summon[Traverse[List]].traverse(ca.tail)(f)) { (h2, t2) =>
      NonEmptyList.cons(h2, t2)
    }

  def foldLeftM[F[+_]: Monad, S, A](ca: NonEmptyList[A])(s: S)(f: (S, A) => F[S]): F[S] =
    f(s, ca.head).flatMap { s2 => summon[Traverse[List]].foldLeftM(ca.tail)(s2)(f) }

  def foldLeft[S, A](ca: NonEmptyList[A])(s: S)(f: (S, A) => S): S = ca.foldLeft(s)(f)

  def reduceLeftM[F[+_]: Monad, A](ca: NonEmptyList[A])(f: (A, A) => F[A]): F[A] =
    summon[Traverse[List]].foldLeftM(ca.tail)(ca.head)(f)

  def reduceLeft[A](ca: NonEmptyList[A])(f: (A, A) => A): A = ca.reduceLeft(f)

  override def map[A, B](fa: NonEmptyList[A])(f: A => B): NonEmptyList[B] = fa.map(f).asInstanceOf[NonEmptyList[B]]
end given
