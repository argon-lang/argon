package dev.argon.util

import zio.*
import zio.stm.*

opaque type MemoCacheStore[R, E, A, B] = MemoCell[Any, Nothing, A => ZIO[R, E, B]]

extension [R, E, A, B](store: MemoCacheStore[R, E, A, B])
  def usingCreate(a: A)(create: A => ZIO[R, E, B]): ZIO[R, E, B] =
    (store : MemoCell[Any, Nothing, A => ZIO[R, E, B]]).get(ZIO.memoize(create)).flatMap { f => f(a) }
end extension

object MemoCacheStore {
  def make[R, E, A, B]: UIO[MemoCacheStore[R, E, A, B]] =
    MemoCell.make
}

