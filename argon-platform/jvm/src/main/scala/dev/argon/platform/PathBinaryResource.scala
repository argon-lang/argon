package dev.argon.platform

import dev.argon.io.*
import zio.*
import zio.stream.ZStream

import java.io.{IOException, InputStream}
import java.nio.channels.SeekableByteChannel
import java.nio.file.{Files, Path}


private[platform] final class PathBinaryResource(path: Path) extends BinaryResource[Any, IOException] {
  override def asBytes: ZStream[Any, IOException, Byte] =
    ZStream.fromPath(path).refineToOrDie[IOException]

  override def asInputStream: ZIO[Scope, IOException, Option[InputStream]] =
    ZIO.fromAutoCloseable(ZIO.attempt { Files.newInputStream(path) }.refineToOrDie[IOException])
      .asSome

  override def asSeekableByteChannel: ZIO[Scope, IOException, Option[SeekableByteChannel]] =
    ZIO.fromAutoCloseable(ZIO.attempt { Files.newByteChannel(path) }.refineToOrDie[IOException])
      .asSome

  override def fileName: Option[String] = Some(path.toString)
}
