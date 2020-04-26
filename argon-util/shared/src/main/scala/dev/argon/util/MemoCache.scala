package dev.argon.util

import zio._

trait MemoCacheStore[E, K, V] {
  def usingCreate[R](create: K => ZIO[R, E, V]): MemoCache[R, E, K, V]
}

object MemoCacheStore {
  def make[E, K, V]: UIO[MemoCacheStore[E, K, V]] =
    for {
      cache <- RefM.make(Map.empty[K, Fiber[E, V]])
    } yield new MemoCacheStore[E, K, V] {
      override def usingCreate[R](create: K => ZIO[R, E, V]): MemoCache[R, E, K, V] = key => for {
        fiber <- cache.modify { fiberMap =>
          fiberMap.get(key) match {
            case Some(fiber) => IO.succeed((fiber, fiberMap))
            case None => create(key).fork.map { fiber => (fiber, fiberMap + (key -> fiber)) }
          }
        }
        value <- fiber.join
      } yield value
    }
}

trait MemoCache[-R, +E, -K, +V] {
  def get(key: K): ZIO[R, E, V]

  def toFunction: K => ZIO[R, E, V] = get
}

object MemoCache {
  def make[R, E, K, V](create: K => ZIO[R, E, V]): UIO[MemoCache[R, E, K, V]] =
    MemoCacheStore.make[E, K, V].map { _.usingCreate(create) }
}
