package dev.argon.expr

import dev.argon.util.{_, given}

sealed trait ExprConstraints[+T] {
  def map[T2 >: T, U](f: T2 => U): ExprConstraints[U]
  def traverse[F[+_]: Applicative, T2 >: T, U](f: T2 => F[U]): F[ExprConstraints[U]]
}

final case class ExprEqualConstraint[+T](other: T) extends ExprConstraints[T] {
  override def map[T2 >: T, U](f: T2 => U): ExprEqualConstraint[U] = ExprEqualConstraint(f(other))

  override def traverse[F[+_]: Applicative, T2 >: T, U](f: T2 => F[U]): F[ExprConstraints[U]] =
    f(other).map(ExprEqualConstraint.apply)

}

final case class ExprTypeBounds[+T]
  (
    superTypeBounds: Seq[T],
    subTypeBounds: Seq[T],
  ) extends ExprConstraints[T] {

  override def map[T2 >: T, U](f: T2 => U): ExprTypeBounds[U] =
    ExprTypeBounds(
      superTypeBounds = superTypeBounds.map(f),
      subTypeBounds = subTypeBounds.map(f),
    )

  override def traverse[F[+_]: Applicative, T2 >: T, U](f: T2 => F[U]): F[ExprConstraints[U]] =
    Applicative[F].map2(
      superTypeBounds.toList.traverse(f),
      subTypeBounds.toList.traverse(f),
    )(ExprTypeBounds.apply)

}
