package dev.argon.util

import zio.*
import zio.stream.*

object ZSinkUtil {
  def single[R, E, A](notSingle: => ZIO[R, E, Nothing]): ZSink[R, E, A, Nothing, A] =
    ZSink.fromChannel(
      ZChannel.readWithCause[R, Nothing, Chunk[A], Any, E, Chunk[Nothing], A](
        in = {
          case _ +: _ +: _ => ZChannel.fromZIO(notSingle)
          case a +: _ => empty(notSingle).as(a).toChannel
          case _ => single(notSingle).toChannel
        },
        halt = ZChannel.failCause(_),
        done = _ => ZChannel.fromZIO(notSingle)
      )
    )

  def empty[R, E, A](notEmpty: => ZIO[R, E, Nothing]): ZSink[R, E, A, Nothing, Unit] =
    ZSink.fromChannel(
      ZChannel.readWithCause[R, Nothing, Chunk[A], Any, E, Chunk[Nothing], Unit](
        in = {
          case _ +: _ => ZChannel.fromZIO(notEmpty)
          case _ => empty(notEmpty).toChannel
        },
        halt = ZChannel.failCause(_),
        done = _ => ZChannel.unit
      )
    )
}
