package dev.argon.stream

import zio.{Chunk, IO, NonEmptyChunk, Ref}
import zio.stream.ZStream

object StreamExtensions {

  implicit class StreamExtensionMethods[R, E, A](private val stream: ZStream[R, E, A]) extends AnyVal {

    def transformWith[R2 <: R, E2 >: E, A2 >: A, B](transformation: StreamTransformation[R2, E2, A2, Unit, B, Unit]): ZStream[R2, E2, B] =
      ZStream.unwrap(
        transformation.start.flatMap(Ref.make)
          .map { state =>
            stream.mapChunksM { chunk =>
              NonEmptyChunk.fromChunk(chunk) match {
                case Some(chunk) =>
                  state.get.flatMap { s =>
                    transformation.consume(s, chunk).flatMap { case (s, outChunk) =>
                      state.set(s).as(outChunk)
                    }
                  }
                case None => IO.succeed(Chunk.empty)
              }
            } ++ ZStream.unwrap(
              state.get
                .flatMap { s => transformation.finish(s, ()) }
                .map { case (chunk, _) => ZStream.fromChunk(chunk) }
            )
          }
      )


  }

}
