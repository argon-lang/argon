package dev.argon.stream

import zio._

trait Resource[F[_], +A] {
  def use[B](f: A => F[B]): F[B]

  def map[B](f: A => B): Resource[F, B] = new Resource[F, B] {
    override def use[C](g: B => F[C]): F[C] =
      Resource.this.use(f andThen g)
  }
}

object Resource {

  def fromZManaged[R, E, A](managed: ZManaged[R, E, A]): Resource[ZIO[R, E, *], A] = new Resource[ZIO[R, E, *], A] {
    override def use[B](f: A => ZIO[R, E, B]): ZIO[R, E, B] = managed.use(f)
  }

  def pure[F[_], A](value: A): Resource[F, A] = new Resource[F, A] {
    override def use[B](f: A => F[B]): F[B] = f(value)
  }

}
