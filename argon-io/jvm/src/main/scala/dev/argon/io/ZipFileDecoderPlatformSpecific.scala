package dev.argon.io

import java.io.IOException
import zio.*
import zio.stream.ZStream
import org.apache.commons.compress.archivers.zip.*
import org.apache.commons.compress.utils.SeekableInMemoryByteChannel

import scala.jdk.CollectionConverters.*
import dev.argon.util.{*, given}

import java.nio.channels.SeekableByteChannel

final class ZipFileDecoderPlatformSpecific extends BinaryResourceDecoder[ZipFileResource, IOException] {
  override def decode[E >: IOException](resource: BinaryResource[E]): ZipFileResource[E] =
    new ZipFileResource[E] {

      override def fileName: Option[String] = None

      override def asBytes: ZStream[Any, E, Byte] = resource.asBytes


      override def asZip: ZIO[Scope, E, ZipFileResource.Zip[E]] =
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


      private final class ZipImpl(zip: ZIO[Scope, E, ZipFile]) extends ZipFileResource.Zip[E] :
        override def getEntry(path: String): ZIO[Scope, E, Option[ZipStreamResource.Entry[E]]] =
          for
            zip <- zip
            lock <- Semaphore.make(1)
            entry <- ZIO.succeed(zip.getEntry(path))
          yield entry.toOption.map(createEntry(zip)(lock))

        override def entries: ZStream[Any, E, ZipStreamResource.Entry[E]] =
          ZStream.unwrapScoped(
            for
              zip <- zip
              lock <- Semaphore.make(1)
              entries <- ZIO.succeed(zip.getEntries.nn.asScala.toSeq)
            yield ZStream.fromIterable(entries.map(createEntry(zip)(lock)))
          )
      end ZipImpl

      private def createEntry(zip: ZipFile)(lock: Semaphore)(entry: ZipArchiveEntry): ZipStreamResource.Entry[E] =
        new ZipStreamResource.Entry[E] {
          override val path: String = entry.getName.nn

          override def value: BinaryResource[E] =
            new BinaryResource[E] {
              override def fileName: Option[String] = None

              override def asBytes: ZStream[Any, E, Byte] =
                ZStream.fromInputStreamScoped(
                  lock.withPermitScoped.as(zip.getInputStream(entry).nn)
                )
            }

        }

    }
  end decode
}
