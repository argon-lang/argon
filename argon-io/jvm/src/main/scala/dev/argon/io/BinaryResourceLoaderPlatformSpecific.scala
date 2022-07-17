package dev.argon.io

import zio.*
import zio.stream.*
import java.io.InputStream
import java.io.IOException
import java.nio.file.Path
import java.nio.file.Files
import java.nio.channels.SeekableByteChannel

trait BinaryResourceLoaderPlatformSpecific {
  def loadInputStream[E >: IOException](inputStream: IO[E, InputStream]): BinaryResource[E] =
    new BinaryResource[E] {
      override def asBytes: Stream[E, Byte] =
        ZStream.unwrapScoped(ZIO.fromAutoCloseable(inputStream).map(ZStream.fromInputStream(_)))

      override def asInputStream: IO[E, Option[InputStream]] =
        inputStream.asSome

      override def fileName: Option[String] = None
    }

  def loadFile(path: Path): BinaryResource[IOException] =
    new BinaryResource[IOException] {
      override def asBytes: Stream[IOException, Byte] =
        ZStream.fromPath(path).refineToOrDie[IOException]

      override def asInputStream: IO[IOException, Option[InputStream]] =
        ZIO.attemptBlockingInterrupt {
          Some(Files.newInputStream(path))
        }.refineToOrDie[IOException]

      override def asSeekableByteChannel: IO[IOException, Option[SeekableByteChannel]] =
        ZIO.attemptBlockingInterrupt {
          Some(Files.newByteChannel(path))
        }.refineToOrDie[IOException]

      override def fileName: Option[String] = Some(path.toString)
    }
    
}
