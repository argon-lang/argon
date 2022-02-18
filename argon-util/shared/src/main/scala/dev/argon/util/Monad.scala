package dev.argon.util

trait Monad[F[+_]] extends Applicative[F]:
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))

  override def ap[A, B](fa: F[A])(fab: F[A => B]): F[B] = flatMap(fab)(f => map(fa)(f))
end Monad

extension [F[+_], A](fa: F[A])(using monad: Monad[F])
  def flatMap[B](f: A => F[B]): F[B] = monad.flatMap(fa)(f)
end extension

object Monad:
  def apply[F[+_]: Monad](using instance: Monad[F]): Monad[F] = instance
end Monad
