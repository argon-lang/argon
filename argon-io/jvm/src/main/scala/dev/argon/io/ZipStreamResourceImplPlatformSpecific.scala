package dev.argon.io

import dev.argon.util.ZStreamFromOutputStreamWriterZIO
import org.apache.commons.compress.archivers.zip.{ZipArchiveEntry, ZipArchiveOutputStream}
import zio.*
import zio.stream.*

import java.io.IOException

trait ZipStreamResourceImplPlatformSpecific[-R, +E >: IOException] extends ZipStreamResource[R, E] {
  override def asBytes: ZStream[R, E, Byte] =
    ZStreamFromOutputStreamWriterZIO { outputStream =>
      ZIO.scoped(
        for
          zipWriter <- ZIO.fromAutoCloseable(ZIO.attempt { ZipArchiveOutputStream(outputStream) }.refineToOrDie[IOException])
          zip <- asZip
          _ <- zip.entries.foreach { entry =>
            for
              _ <- ZIO.attemptBlockingIO {
                zipWriter.putArchiveEntry(ZipArchiveEntry(entry.path))
              }
              _ <- entry.value.asBytes.run(ZSink.fromOutputStream(zipWriter))
              _ <- ZIO.attemptBlockingIO { zipWriter.closeArchiveEntry() }
            yield ()
          }
        yield ()
      )
    }
}
