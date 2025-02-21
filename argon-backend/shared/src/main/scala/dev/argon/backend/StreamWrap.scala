package dev.argon.backend

import dev.argon.backend.scalaApi.ScopedResource
import dev.argon.util.async.ErrorWrapper
import zio.*
import zio.stream.*

private[backend] object StreamWrap {
  def wrapStream[E, A](stream: Stream[E, A])(using ew: ErrorWrapper[E]): IO[ew.EX, ScopedResource[scalaApi.Stream[ew.EX, A]]] =
    asScopedResource(
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
      ZStream.scoped(stream.withFinalizer(_.close()).flatMap(_.get()))
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


  private def asScopedResource[E, A](io: ZIO[Scope, E, A]): IO[E, ScopedResource[A]] =
    Scope.make.flatMap { scope =>
      io.provideEnvironment(ZEnvironment(scope))
        .foldCauseZIO(
          failure = cause => scope.close(Exit.failCause(cause)) *> ZIO.failCause(cause),
          success = a => ZIO.succeed(new ScopedResource[A] {
            override def get(): UIO[A] = ZIO.succeed(a)

            override def close(): UIO[Unit] = scope.close(Exit.unit)
          })
        )
    }
}
