package dev.argon.backend

import dev.argon.backend.scalaApi.ScopedResource
import dev.argon.util.async.ErrorWrapper
import zio.*
import zio.stream.*

private[backend] object StreamWrap {
  def wrapStream[E, A](stream: Stream[E, A])(using ew: ErrorWrapper[E]): IO[ew.EX, ScopedResource[scalaApi.Stream[ew.EX, A]]] =
    ScopedResourceWrap.wrap(
      for
        pull <- ErrorWrapper.wrapStream(stream).toPull
      yield new scalaApi.Stream[ew.EX, A] {
        override def next(): IO[ew.EX, Seq[A]] =
          pull.foldZIO(
            failure = {
              case Some(e) => ZIO.fail(e)
              case None => ZIO.succeed(Seq.empty)
            },
            success = chunk => {
              if chunk.isEmpty then
                next()
              else
                ZIO.succeed(chunk)
            },
          )
      }
    )

  def unwrapStream[E, A](using ew: ErrorWrapper[E])(stream: IO[ew.EX, ScopedResource[scalaApi.Stream[ew.EX, A]]]): Stream[E, A] =
    ErrorWrapper.unwrapStream(
      ZStream.scoped(ScopedResourceWrap.unwrap(stream))
        .flatMap { stream =>
          ZStream.fromPull(
            ZIO.succeed(
              stream.next().asSomeError
                .flatMap { items =>
                  if items.isEmpty then
                    ZIO.fail(None)
                  else
                    ZIO.succeed(Chunk.fromIterable(items))
                }
            )
          )
        }
    )
  
}
