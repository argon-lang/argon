package dev.argon.io

import dev.argon.stream._
import java.io.{IOException, InputStream}

import cats.Monad
import cats.data.NonEmptyVector
import zio.blocking.Blocking
import zio.stream.ZStream
import zio.stream.ZStream.Pull
import zio.{Chunk, IO, ZIO, ZManaged}

final class InputStreamStream[-R, +E] private(val inputStream: ZManaged[R, E, InputStream], process: ZManaged[R, E, Pull[R, E, Chunk[Byte]]]) extends ZStream[R, E, Chunk[Byte]](process)

object InputStreamStream {

  def apply[R <: Blocking, E](errorHandler: IOException => E)(inputStream: ZManaged[R, E, InputStream]): InputStreamStream[R, E] = {
    val process = inputStream.flatMap { is =>
      val zstream = ZStream.fromInputStream(is).chunks

      zstream.process.bimap(
        errorHandler,
        pull => pull.mapError(_.map(errorHandler))
      )
    }

    new InputStreamStream[R, E](inputStream, process)
  }

}
