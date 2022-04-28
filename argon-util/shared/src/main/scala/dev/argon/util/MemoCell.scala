package dev.argon.util

import zio.*

opaque type MemoCell[R, E, A] = Ref.Synchronized[Option[ZIO[R, E, A]]]

extension [R, E, A](cell: MemoCell[R, E, A])

  def get(io: => ZIO[R, E, A]): ZIO[R, E, A] =
    cell.modifyZIO {
      case res @ Some(eff) => IO.succeed((eff, res))
      case None => io.memoize.map { res => (res, Some(res)) }
    }.flatten

end extension

object MemoCell {
  def make[R, E, A]: UIO[MemoCell[R, E, A]] = Ref.Synchronized.make(None)
}
