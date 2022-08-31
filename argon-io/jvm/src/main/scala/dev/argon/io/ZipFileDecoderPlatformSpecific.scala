package dev.argon.io

import dev.argon.util.Nullable

import java.io.IOException
import zio.*
import zio.stream.ZStream
import org.apache.commons.compress.archivers.zip.*
import org.apache.commons.compress.utils.SeekableInMemoryByteChannel

import scala.jdk.CollectionConverters.*
import dev.argon.util.{*, given}

import java.nio.channels.SeekableByteChannel

final class ZipFileDecoderPlatformSpecific extends BinaryResourceDecoder[ZipFileResource, Any, IOException] {
  override def decode[R <: Any, E >: IOException](resource: BinaryResource[R, E]): ZipFileResource[R, E] =
    new ZipFileResource[R, E] {

      override def fileName: Option[String] = None

      override def asBytes: ZStream[R, E, Byte] = resource.asBytes


      override def asZip: ZIO[R & Scope, E, ZipFileResource.Zip[R, E]] =
        resource.asSeekableByteChannel match {
          case Some(channel) =>
            for
              numCores <- ZIO.succeed {
                java.lang.Runtime.getRuntime.nn.availableProcessors()
              }
              pool <- ZPool.make(channel.map(ZipFile(_)), numCores)
            yield ZipImpl(pool.get)

          case None =>
            resource
              .asBytes
              .runCollect
              .map {
                _.toArray
              }
              .memoize
              .map { bytes =>
                ZipImpl(
                  bytes.flatMap { byteArray =>
                    ZIO.attempt(ZipFile(SeekableInMemoryByteChannel(byteArray))).refineToOrDie[IOException]
                  }
                )
              }
        }


      private final class ZipImpl(zip: ZIO[R & Scope, E, ZipFile]) extends ZipFileResource.Zip[R, E] :
        override def getEntry(path: String): ZIO[R & Scope, E, Option[ZipFileResource.Entry[R, E]]] =
          for
            zip <- zip
            lock <- Semaphore.make(1)
            entry <- ZIO.succeed(zip.getEntry(path))
          yield Nullable(entry).toOption.map(createEntry(zip)(lock))

        override def entries: ZStream[R, E, ZipFileResource.Entry[R, E]] =
          ZStream.unwrapScoped(
            for
              zip <- zip
              lock <- Semaphore.make(1)
              entries <- ZIO.succeed(zip.getEntries.nn.asScala.toSeq)
            yield ZStream.fromIterable(entries.map(createEntry(zip)(lock)))
          )
      end ZipImpl

      private def createEntry(zip: ZipFile)(lock: Semaphore)(entry: ZipArchiveEntry): ZipFileResource.Entry[R, E] =
        new ZipFileResource.Entry[R, E] {
          override val path: String = entry.getName.nn

          override def value: BinaryResource[R, E] =
            new BinaryResource[R, E] {
              override def fileName: Option[String] = None

              override def asBytes: ZStream[R, E, Byte] =
                ZStream.fromInputStreamScoped(
                  lock.withPermitScoped.as(zip.getInputStream(entry).nn)
                )
            }

        }

    }
  end decode
}
