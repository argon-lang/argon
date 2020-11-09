package dev.argon.util

import zio._

trait MemoCacheManagedStore[E, K, V] {
  def usingCreate[R](create: K => ZManaged[R, E, V]): MemoCacheManaged[R, E, K, V]
}

object MemoCacheManagedStore {
  def make[E, K, V]: UManaged[MemoCacheManagedStore[E, K, V]] =
    for {
      cache <- ZManaged.make(
        RefM.make(Map.empty[K, (Fiber[E, V], Exit[Any, Any] => UIO[Any])])
      )(
        cache => cache.get.flatMap { cacheMap =>
          ZIO.foldLeft(cacheMap.values)(Exit.unit) { case (exit, (fiber, release)) =>
            fiber.await
              .flatMap { exit =>
                release(exit)
              }
            .foldCause(
              failure = cause => Exit.Failure(exit match {
                case Exit.Success(_) => cause
                case Exit.Failure(prevCause) => prevCause ++ cause
              }),
              success = _ => exit
            )

          }
        }.flatMap { exit => IO.done(exit) }
      )
    } yield new MemoCacheManagedStore[E, K, V] {
      override def usingCreate[R](create: K => ZManaged[R, E, V]): MemoCacheManaged[R, E, K, V] = key => for {
        env <- ZIO.environment[R]
        fiber <- cache.modify { fiberMap =>
          fiberMap.get(key) match {
            case Some((fiber, _)) => IO.succeed((fiber, fiberMap))
            case None =>
              create(key)
                .reserve
                .flatMap { reservation =>
                  val release = reservation.release
                  reservation.acquire
                    .fork
                    .map { fiber => (fiber, fiberMap.updated(key, (fiber, (exit: Exit[Any, Any]) => release(exit).provide(env)))) }
                }
          }
        }
        value <- fiber.join
      } yield value
    }
}

trait MemoCacheManaged[-R, +E, -K, +V] {
  def get(key: K): ZIO[R, E, V]

  def toFunction: K => ZIO[R, E, V] = get
}

object MemoCacheManaged {
  def make[R, E, K, V](create: K => ZManaged[R, E, V]): UManaged[MemoCacheManaged[R, E, K, V]] =
    MemoCacheManagedStore.make[E, K, V].map { _.usingCreate(create) }
}
