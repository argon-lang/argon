package dev.argon.util.stream

import scalaz.zio._

trait Resource[F[-_, +_, +_], -R, +E, +A] {
  def use[R2 <: R, E2 >: E, B](f: A => F[R2, E2, B]): F[R2, E2, B]
}

object Resource {

  def fromZManaged[R, E, A](managed: ZManaged[R, E, A]): Resource[ZIO, R, E, A] = new Resource[ZIO, R, E, A] {
    override def use[R2 <: R, E2 >: E, B](f: A => ZIO[R2, E2, B]): ZIO[R2, E2, B] = managed.use(f)
  }

}
