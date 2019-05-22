package dev.argon.util.stream

import scalaz.zio._

trait Resource[F[+_, +_], +R, +E] {
  def use[E2 >: E, A](f: R => F[E2, A]): F[E2, A]
}

object Resource {

  def fromZManaged[E, R](managed: Managed[E, R]): Resource[IO, R, E] = new Resource[IO, R, E] {
    override def use[E2 >: E, A](f: R => IO[E2, A]): IO[E2, A] = managed.use(f)
  }

}
