package dev.argon.plugin

import dev.argon.plugin.{api => japi}
import dev.argon.io.DirectoryResource
import java.nio.channels.SeekableByteChannel
import java.io.InputStream
import zio.*
import zio.stream.*
import dev.argon.util.{*, given}
import java.io.IOException
import scala.reflect.TypeTest
import dev.argon.io.DirectoryEntry

private[plugin] final class JavaDirectoryResourceUnwrap[E >: IOException, EX <: Exception](resource: japi.resource.DirectoryResource[EX])(using ErrorWrapper[E, EX], TypeTest[Throwable, EX]) extends DirectoryResource[E] {
  override def contents: Stream[E, DirectoryEntry[E]] =
    ZStream.fromJavaStreamZIO(IO.attemptBlockingInterrupt { resource.contents })
      .catchAll(JavaErrorHandler.handleErrorsStream)
      .map { entry =>
        val resource = entry.resource match {
          case resource: japi.resource.BinaryResource[EX] =>
            new JavaBinaryResourceUnwrap[E, EX](resource)

          case resource: japi.resource.DirectoryResource[EX] =>
            new JavaDirectoryResourceUnwrap[E, EX](resource)

          case resource => throw new MatchError(resource)
        }

        DirectoryEntry(entry.name, resource)
      }
      
}
