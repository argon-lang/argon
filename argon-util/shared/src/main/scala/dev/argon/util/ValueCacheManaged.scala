package dev.argon.util

import zio._

trait ValueCacheManaged[E, A] {
  def get[R](create: ZManaged[R, E, A]): ZIO[R, E, A]
}

object ValueCacheManaged {
  def make[E, A]: UManaged[ValueCacheManaged[E, A]] = for {
    ref <- ZManaged.make(
      RefM.make[Option[(Fiber[E, A], Exit[Any, Any] => UIO[Any])]](None)
    )(
      ref => ref.get.flatMap {
        case Some((fiber, release)) =>
          fiber.await.flatMap { exit =>
            release(exit)
          }
        case None => IO.unit
      }
    )
  } yield new ValueCacheManaged[E, A] {
    override def get[R](create: ZManaged[R, E, A]): ZIO[R, E, A] = for {
      env <- ZIO.environment[R]
      fiber <- ref.modify {
        case None =>
          create
            .reserve
            .flatMap { reservation =>
              val release = reservation.release
              reservation.acquire
                .fork
                .map { fiber => (fiber, Some((fiber, (exit: Exit[Any, Any]) => release(exit).provide(env)))) }
            }

        case Some(value @ (fiber, _)) => IO.succeed((fiber, Some(value)))
      }.uninterruptible

      a <- fiber.join
    } yield a
  }

}

