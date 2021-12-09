package dev.argon.io

import zio.stream._
import zio._
import java.nio.channels.SeekableByteChannel
import java.nio.file.{Path, Files, StandardOpenOption}

trait ResourceId {
  def asStream: ZStream[Any, Nothing, Byte]
  def asSeekableByteChannel: Option[ZManaged[Any, Nothing, SeekableByteChannel]] = None
}

object ResourceId {

  def fromPath(path: Path): ResourceId =
    new ResourceId {
      override def asStream: ZStream[Any, Nothing, Byte] = ZStream.fromFile(path).refineOrDie(PartialFunction.empty)

      override def asSeekableByteChannel: Option[ZManaged[Any, Nothing, SeekableByteChannel]] =
        Some(ZManaged.fromAutoCloseable(ZIO.succeed(Files.newByteChannel(path, StandardOpenOption.READ))))

    }

}
