package dev.argon.util

import zio.*
import zio.stm.*

opaque type MemoCacheStore[R, E, A, B] = TMap[A, ZIO[R, E, B]]

extension [R, E, A, B](store: MemoCacheStore[R, E, A, B])
  def usingCreate(a: A)(create: A => ZIO[R, E, B]): ZIO[R, E, B] =
    ZIO.suspendSucceed { create(a) }
      .memoize
      .flatMap { createMemo =>
        store.get(a)
          .flatMap {
            case Some(value) => ZSTM.succeed(value)
            case None => store.put(a, createMemo).as(createMemo)
          }
          .commit
      }
      .flatten
end extension

object MemoCacheStore {
  def make[R, E, A, B]: UIO[MemoCacheStore[R, E, A, B]] =
    TMap.empty.commit
}

