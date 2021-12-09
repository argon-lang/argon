package dev.argon.io

import dev.argon.util.given
import zio._
import zio.stream._
import org.apache.commons.compress.utils.SeekableInMemoryByteChannel
import org.apache.commons.compress.archivers.zip.{ZipFile => ACZipFile}
import java.nio.channels.SeekableByteChannel
import org.apache.commons.compress.archivers.zip.ZipArchiveEntry

object ZipFileResourceLoader extends ResourceLoader[ZipFileResource] {

  override def load(id: ResourceId): ZipFileResource =
    val channel =
      id.asSeekableByteChannel.getOrElse {
        ZManaged.fromZIO(
          id.asStream.runCollect.flatMap { chunk =>
            IO.succeed { new SeekableInMemoryByteChannel(chunk.toArray) }
          }
        )
      }
    loadChannel(id, channel)
  end load

  private def loadChannel(id: ResourceId, channel: UManaged[SeekableByteChannel]): ZipFileResource =
    new ZipFileResource {

      override def zipFile: UManaged[ZipFile] =
        channel.mapZIO { channel => IO.attemptBlockingInterrupt { new ACZipFile(channel) }.orDie }.map(new ZipFileImpl(_))

      override def asBytes: UStream[Byte] = id.asStream
    }

  private final class ZipFileImpl(zip: ACZipFile) extends ZipFile {

    override def entry(path: String): UIO[Option[ZipEntry]] =
      IO.succeed { Option(zip.getEntry(path)).map(new ZipEntryImpl(zip, _)) }

    override def entries: UStream[ZipEntry] =
      ZStream.fromJavaIteratorZIO(IO.succeed { zip.getEntries.asIterator })
        .map(new ZipEntryImpl(zip, _))
        .refineOrDie(PartialFunction.empty)

  }

  private final class ZipEntryImpl(zip: ACZipFile, entry: ZipArchiveEntry) extends ZipEntry {
    override def path: String = entry.getName

    override def read: UStream[Byte] =
      ZStream.fromInputStreamZIO(IO.attemptBlockingInterrupt { zip.getInputStream(entry) }.orDie)
        .refineOrDie(PartialFunction.empty)

  }

}
