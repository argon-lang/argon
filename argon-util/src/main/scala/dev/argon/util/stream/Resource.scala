package dev.argon.util.stream

import cats._
import scalaz.zio._

trait Resource[F[-_, +_, +_], -R, +E, +A] {
  def use[R2 <: R, E2 >: E, B](f: A => F[R2, E2, B]): F[R2, E2, B]
  def useIO[R2 <: R, E2 >: E, B](f: A => ZIO[R2, E2, B]): ZIO[R2, E2, B]

  def mapError[E2](f: E => E2): Resource[F, R, E2, A]

  def map[R2 <: R, E2 >: E, B](f: A => B): Resource[F, R2, E2, B] = new Resource[F, R2, E2, B] {
    override def use[R3 <: R2, E3 >: E2, C](g: B => F[R3, E3, C]): F[R3, E3, C] =
      Resource.this.use(f andThen g)

    override def useIO[R3 <: R2, E3 >: E2, C](g: B => ZIO[R3, E3, C]): ZIO[R3, E3, C] =
      Resource.this.useIO(f andThen g)

    override def mapError[E3](g: E2 => E3): Resource[F, R2, E3, B] =
      Resource.this.mapError(g).map(f)
  }
}

object Resource {

  def fromZManaged[R, E, A](managed: ZManaged[R, E, A]): Resource[ZIO, R, E, A] = new Resource[ZIO, R, E, A] {
    override def use[R2 <: R, E2 >: E, B](f: A => ZIO[R2, E2, B]): ZIO[R2, E2, B] = managed.use(f)
    override def useIO[R2 <: R, E2 >: E, B](f: A => ZIO[R2, E2, B]): ZIO[R2, E2, B] = managed.use(f)

    override def mapError[E2](f: E => E2): Resource[ZIO, R, E2, A] =
      fromZManaged(managed.mapError(f))
  }

  def pure[F[-_, +_, +_], A](value: A): Resource[F, Any, Nothing, A] = new Resource[F, Any, Nothing, A] {
    override def use[R2 <: Any, E2 >: Nothing, B](f: A => F[R2, E2, B]): F[R2, E2, B] = f(value)
    override def useIO[R2 <: Any, E2 >: Nothing, B](f: A => ZIO[R2, E2, B]): ZIO[R2, E2, B] = f(value)
    override def mapError[E2](f: Nothing => E2): Resource[F, Any, E2, A] = this
  }

}
