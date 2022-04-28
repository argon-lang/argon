package dev.argon.plugin

import dev.argon.plugin.{api => japi}
import dev.argon.io.BinaryResource
import java.nio.channels.SeekableByteChannel
import java.io.InputStream
import zio.*
import zio.stream.*
import dev.argon.util.{*, given}
import java.io.IOException
import scala.reflect.TypeTest

final class JavaBinaryResourceUnwrap[E >: IOException, EX <: Exception](resource: japi.resource.BinaryResource[EX])(using ErrorWrapper[E, EX], TypeTest[Throwable, EX]) extends BinaryResource[E] {
  override def asBytes: Stream[E, Byte] =
    JavaInputStreamUnwrap.toZStream(resource.asInputStream)

  override def asInputStream: IO[E, Option[InputStream]] =
    IO.attemptBlockingInterrupt {
      Some(resource.asInputStream)
    }.catchAll(JavaErrorHandler.handleErrors)

  override def asSeekableByteChannel: IO[E, Option[SeekableByteChannel]] =
    IO.attemptBlockingInterrupt {
      Option(resource.asSeekableByteChannel)
    }.catchAll(JavaErrorHandler.handleErrors)

}
