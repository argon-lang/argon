package dev.argon.util

import zio.stream.ZChannel
import zio.Chunk

object ZChannelUtil {

  def mapAccumChunks[E, A, B, S](state: S)(f: (S, A) => (S, B)): ZChannel[Any, E, A, Any, E, B, S] =
    def processChunk(state: S)(a: A): ZChannel[Any, E, A, Any, E, B, S] =
      val (state2, results) = f(state, a)
      ZChannel.write(results) *> process(state2)
    end processChunk

    def process(state: S): ZChannel[Any, E, A, Any, E, B, S] =
      ZChannel.readWithCause(
        in = processChunk(state),
        halt = ZChannel.failCause(_),
        done = _ => ZChannel.succeed(state),
      )

    process(state)
  end mapAccumChunks

  def mapAccum[E, A, B, S](state: S)(f: (S, A) => (S, B)): ZChannel[Any, E, Chunk[A], Any, E, Chunk[B], S] =
    mapAccumChunks(state) { (s, a) => a.mapAccum(s)(f) }

  def mapAccumMany[E, A, B, S](state: S)(f: (S, A) => (S, Chunk[B])): ZChannel[Any, E, Chunk[A], Any, E, Chunk[B], S] =
    mapAccum(state)(f).pipeTo(mapElements(_.flatten))

  def mapAccumOption[E, A, B, S](state: S)(f: (S, A) => (S, Option[B]))
    : ZChannel[Any, E, Chunk[A], Any, E, Chunk[B], S] = mapAccum(state)(f).pipeTo(mapElements(_.flatten))

  def mapElements[E, A, B, X](f: A => B): ZChannel[Any, E, A, X, E, B, X] =
    ZChannel.readWithCause(
      in = a => ZChannel.write(f(a)) *> mapElements(f),
      halt = ZChannel.failCause(_),
      done = ZChannel.succeed(_),
    )

}
