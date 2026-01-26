package dev.argon.backend.scalaApi

import zio.*
import ScopedResourceExtensions.*

object StreamUtil {
  
  def fromZStreamScoped[E, A](s: zio.stream.Stream[E, A]): UIO[ScopedResource[E, Stream[E, A]]] =
    ScopedResource.fromScopeIO(
      for
        pull <- s.toPull
      yield new Stream[E, A] {
        override def next(): IO[E, Seq[A]] =
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
  
  def toZStream[E, A](s: Stream[E, A]): zio.stream.Stream[E, A] =
    zio.stream.ZStream.fromPull(
      ZIO.succeed(
        s.next()
          .asSomeError
          .flatMap { items =>
            if items.isEmpty then
              ZIO.fail(None)
            else
              ZIO.succeed(Chunk.fromIterable(items))
          }
      )
    )
    
  def toZStreamScoped[E, A](s: ScopedResource[E, Stream[E, A]]): zio.stream.Stream[E, A] =
    zio.stream.ZStream.unwrapScoped(s.toScopeIO.map(toZStream))

}
