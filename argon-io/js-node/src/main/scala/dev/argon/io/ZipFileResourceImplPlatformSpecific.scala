package dev.argon.io

import zio.*
import zio.stream.*

import java.io.IOException

trait ZipFileResourceImplPlatformSpecific[-R, +E >: IOException] extends ZipFileResource[R, E] {
  override def asBytes: ZStream[R, E, Byte] =
    ZStream.unwrapScoped(
      for
        jszip <- ZIO.succeed { JSZip() }
        zip <- asZip
        _ <- zip.entries.foreach { entry =>
          dataStreamToUint8Array(entry.value.asBytes).flatMap { data =>
            ZIO.succeed { jszip.file(entry.path, data) }
          }
        }
        zipData <- ZIO.fromPromiseJS {
          jszip.generateAsync(
            new JSZip.JSZipGeneratorOptions {
              override val `type`: "uint8array" = "uint8array"
            }
          )
        }.orDie
        zipDataChunk <- uint8ArrayToChunk(zipData)
      yield ZStream.fromChunk(zipDataChunk)
    )
}
