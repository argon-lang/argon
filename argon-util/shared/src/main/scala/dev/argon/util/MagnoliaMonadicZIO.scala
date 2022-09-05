package dev.argon.util

import magnolia1.*
import zio.*


given MagnoliaMonadicZIO[R, E]: Monadic[[A] =>> ZIO[R, E, A]] with
  override def point[A](value: A): ZIO[R, E, A] = ZIO.succeed(value)

  override def map[A, B](from: ZIO[R, E, A])(fn: A => B): ZIO[R, E, B] =
    from.map(fn)

  override def flatMap[A, B](from: ZIO[R, E, A])(fn: A => ZIO[R, E, B]): ZIO[R, E, B] =
    from.flatMap(fn)
end MagnoliaMonadicZIO
