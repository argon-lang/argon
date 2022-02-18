package dev.argon.io

import dev.argon.util.given
import zio.*
import zio.stream.*
import org.apache.commons.compress.utils.SeekableInMemoryByteChannel
import org.apache.commons.compress.archivers.zip.ZipFile as ACZipFile
import java.nio.channels.SeekableByteChannel
import org.apache.commons.compress.archivers.zip.ZipArchiveEntry
import java.io.IOException
import java.nio.file.Files
import java.nio.file.StandardOpenOption

object ZipFileResourceLoader extends ResourceLoader[ZipFileResource] {

  def loadResourceId: PartialFunction[ResourceId, ZipFileResource] = {
    case FileNameResourceId(path) =>
      val channel =
        ZManaged.fromAutoCloseable(
          IO.attempt { Files.newByteChannel(path, StandardOpenOption.READ) }
            .refineToOrDie[IOException]
        )

      new ChannelZipResource(channel)
  }

  def loadStream(stream: Stream[IOException, Byte]): ZipFileResource =
    val channel =
      ZManaged.fromZIO(
        stream.runCollect.flatMap { chunk =>
          IO.succeed { new SeekableInMemoryByteChannel(chunk.toArray) }
        }
      )
    new ChannelZipResource(channel) {
      override def asBytes: Stream[IOException, Byte] = stream
    }
  end loadStream

  private class ChannelZipResource(channel: Managed[IOException, SeekableByteChannel]) extends ZipFileResource {

    override def zipFile: Managed[IOException, ZipFile[Any, IOException]] =
      channel
        .mapZIO { channel =>
          IO.attemptBlockingInterrupt { new ACZipFile(channel) }
            .refineToOrDie[IOException]
        }
        .map(new ZipFileImpl(_))

  }

  private final class ZipFileImpl(zip: ACZipFile) extends ZipFile[Any, IOException] {

    override def entry(path: String): UIO[Option[ZipEntry[Any, IOException]]] =
      IO.succeed { Option(zip.getEntry(path)).map(new ZipEntryImpl(zip, _)) }

    override def entries: UStream[ZipEntry[Any, IOException]] =
      ZStream.fromJavaIteratorZIO(IO.succeed { zip.getEntries.asIterator })
        .map(new ZipEntryImpl(zip, _))
        .refineOrDie(PartialFunction.empty)

  }

  private final class ZipEntryImpl(zip: ACZipFile, entry: ZipArchiveEntry) extends ZipEntry[Any, IOException] {
    override def path: String = entry.getName

    override def read: ZStream[Any, IOException, Byte] =
      ZStream.fromInputStreamZIO(IO.attemptBlockingInterrupt { zip.getInputStream(entry) }.orDie)
        .refineOrDie(PartialFunction.empty)

  }

}
