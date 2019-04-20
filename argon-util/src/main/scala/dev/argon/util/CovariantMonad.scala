package dev.argon.util

import scalaz._
import Scalaz._

trait CovariantMonad[F[_], +A] {
  def run[B >: A]: F[B]

  def map[B](f: A => B): CovariantMonad[F, B]
  def flatMap[B](f: A => CovariantMonad[F, B]): CovariantMonad[F, B]
}

object CovariantMonad {

  def apply[F[_] : Monad, A](value: F[A]): CovariantMonad[F, A] = new CovariantMonad[F, A] {
    override def run[B >: A]: F[B] = value.map(identity)

    override def map[B](f: A => B): CovariantMonad[F, B] =
      CovariantMonad(value.map(f))

    override def flatMap[B](f: A => CovariantMonad[F, B]): CovariantMonad[F, B] =
      CovariantMonad(value.flatMap(a => f(a).run))
  }

  implicit def monadInstance[F[_] : Monad]: Monad[CovariantMonad[F, ?]] = new Monad[CovariantMonad[F, ?]] {
    override def point[A](a: => A): CovariantMonad[F, A] = CovariantMonad(Monad[F].point(a))

    override def bind[A, B](fa: CovariantMonad[F, A])(f: A => CovariantMonad[F, B]): CovariantMonad[F, B] =
      fa.flatMap(f)
  }

}
