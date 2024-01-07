package dev.argon.util

import zio.stream.ZChannel
import zio.*

object ZChannelUtil {


  def mapAccumChunksZIO[R, E, A, B, S](state: S)(f: (S, A) => ZIO[R, E, (S, B)]): ZChannel[R, E, A, Any, E, B, S] =
    def processChunk(state: S)(a: A): ZChannel[R, E, A, Any, E, B, S] =
      ZChannel.unwrap(
        f(state, a).map { (state, results) =>
          ZChannel.write(results) *> process(state)
        }
      )

    def process(state: S): ZChannel[R, E, A, Any, E, B, S] =
      ZChannel.readWithCause(
        in = processChunk(state),
        halt = ZChannel.failCause(_),
        done = _ => ZChannel.succeed(state),
      )

    process(state)
  end mapAccumChunksZIO
  
  def mapAccumChunks[A, B, S](state: S)(f: (S, A) => (S, B)): ZChannel[Any, Nothing, A, Any, Nothing, B, S] =
    def processChunk(state: S)(a: A): ZChannel[Any, Nothing, A, Any, Nothing, B, S] =
      val (state2, results) = f(state, a)
      ZChannel.write(results) *> process(state2)
    end processChunk

    def process(state: S): ZChannel[Any, Nothing, A, Any, Nothing, B, S] =
      ZChannel.readWithCause(
        in = processChunk(state),
        halt = ZChannel.failCause(_),
        done = _ => ZChannel.succeed(state),
      )

    process(state)
  end mapAccumChunks

  def mapAccum[A, B, S](state: S)(f: (S, A) => (S, B)): ZChannel[Any, Nothing, Chunk[A], Any, Nothing, Chunk[B], S] =
    mapAccumChunks(state) { (s, a) => a.mapAccum(s)(f) }

  def mapAccumOption[A, B, S](state: S)(f: (S, A) => (S, Option[B]))
    : ZChannel[Any, Nothing, Chunk[A], Any, Nothing, Chunk[B], S] = mapAccum(state)(f).mapOut(_.flatten)

}
