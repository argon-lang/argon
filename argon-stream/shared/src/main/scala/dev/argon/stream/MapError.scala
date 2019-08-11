package dev.argon.stream

trait MapError[F[_, _, _]] {

  def mapError[R, E, E2, A](fe: F[R, E, A])(f: E => E2): F[R, E2, A]

}
