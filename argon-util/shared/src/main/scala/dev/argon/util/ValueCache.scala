package dev.argon.util

import zio.{Fiber, IO, RefM, UIO, UManaged, ZIO}

trait ValueCache[E, A] {
  def get[R](create: ZIO[R, E, A]): ZIO[R, E, A]
}

object ValueCache {
  def make[E, A]: UIO[ValueCache[E, A]] =
    RefM.make[Option[Fiber[E, A]]](None).map { ref =>
      new ValueCache[E, A] {
        override def get[R](create: ZIO[R, E, A]): ZIO[R, E, A] =
          ref.modify {
            case None => create.fork.map { fiber => (fiber, Some(fiber)) }
            case Some(value) => IO.succeed((value, Some(value)))
          }
          .flatMap { _.join }
      }
    }

}
