package dev.argon.io

import zio.stream.*
import dev.argon.util.ZStreamFromOutputStreamWriterZIO
import org.apache.commons.compress.archivers.zip.ZipArchiveOutputStream
import zio.*
import java.io.IOException

private[io] object ZipFilePlatform {

  def serializeZipFile(zipFile: ZipFile[Any, IOException]): Stream[IOException, Byte] =
    ZStreamFromOutputStreamWriterZIO { stream =>
      for {
        zip <- ZIO.attempt { new ZipArchiveOutputStream(stream) }.refineToOrDie[IOException]
        _ <-
          zipFile.entries.foreach { entry =>
            entry.read.foreachChunk { chunk =>
              ZIO.attemptBlockingInterrupt {
                stream.write(chunk.toArray)
              }.refineToOrDie[IOException]
            }
          }
      } yield ()
    }

}
