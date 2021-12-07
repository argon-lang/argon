package dev.argon.util

trait Functor[F[+_]]:
  def map[A, B](fa: F[A])(f: A => B): F[B]
end Functor

extension [F[+_], A](fa: F[A])(using functor: Functor[F])
  def map[B](f: A => B): F[B] = functor.map(fa)(f)
end extension
