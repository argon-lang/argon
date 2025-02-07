package dev.argon.util

import zio.*
import zio.stream.*

object ZChannelUtils {
  def peel[R, E, A, B](
    process: A => ZChannel[R, Nothing, A, Any, E, Nothing, B],
    empty: => ZChannel[R, Nothing, A, Any, E, Nothing, B],
  ): ZChannel[R, Nothing, A, Any, E, Nothing, B] =
    ZChannel.readWithCause(
      in = process,
      halt = ZChannel.failCause(_),
      done = _ => empty
    )

  def unchunkInput[R, A, Z]: ZChannel[R, Nothing, Chunk[A], Z, Nothing, A, Z] =
    ZChannel.readWithCause(
      in = chunk =>
        chunk.foldLeft[ZChannel[R, Nothing, Chunk[A], Z, Nothing, A, Unit]](ZChannel.unit) { (acc, a) =>
          acc *> ZChannel.write(a)
        } *> unchunkInput[R, A, Z],
      halt = ZChannel.failCause(_),
      done = ZChannel.succeed(_),
    )
}
