package dev.argon.io

import dev.argon.io.*
import dev.argon.util.async.ErrorWrapper
import zio.*
import zio.stream.ZStream

import java.io.{IOException, InputStream}
import java.nio.channels.SeekableByteChannel
import java.nio.file.{Files, Path}


private[io] final class PathBinaryResource(path: Path) extends BinaryResource[IOException] {
  override def asBytes: ZStream[Any, IOException, Byte] =
    ZStream.fromPath(path).refineToOrDie[IOException]

  override def asInputStream[E1 >: IOException](using ErrorWrapper[E1]): ZIO[Scope, E1, InputStream] =
    ZIO.fromAutoCloseable(ZIO.attempt { Files.newInputStream(path).nn }
      .refineToOrDie[IOException])

  override def fileName: Option[String] = Some(path.toString)
}
