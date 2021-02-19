package dev.argon.io

import zio._
import zio.stream._

trait ZipFileReader[+E] {
  def getEntryStream(name: String): IO[E, Option[Stream[E, Byte]]]

  final def catchAll[E2](f: E => IO[E2, Nothing]): ZipFileReader[E2] = new ZipFileReader[E2] {
    override def getEntryStream(name: String): IO[E2, Option[Stream[E2, Byte]]] =
      ZipFileReader.this.getEntryStream(name)
        .foldM(
          failure = f,
          success = opt => IO.succeed(opt.map { stream =>
            stream.catchAll { ex => ZStream.fromEffect(f(ex)) }
          })
        )
  }
}
