package dev.argon.io

import zio.stream.*
import dev.argon.util.ZStreamFromOutputStreamWriterZIO
import org.apache.commons.compress.archivers.zip.ZipArchiveOutputStream
import zio.*

private[io] object ZipFilePlatform {

  def serializeZipFile(zipFile: ZipFile): UStream[Byte] =
    ZStreamFromOutputStreamWriterZIO { stream =>
      for {
        zip <- ZIO.succeed { ZipArchiveOutputStream(stream) }
        _ <-
          zipFile.entries.foreach { entry =>
            entry.read.foreachChunk { chunk =>
              ZIO.attemptBlockingInterrupt {
                stream.write(chunk.toArray)
              }.orDie
            }
          }
      } yield ()
    }

}
