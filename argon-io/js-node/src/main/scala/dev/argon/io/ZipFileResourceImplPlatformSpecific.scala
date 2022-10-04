package dev.argon.io

import zio.*
import zio.stream.*

import java.io.IOException
import typings.jszip.mod.{Class as JSZipClass, JSZipGeneratorOptions}
import typings.jszip.jszipStrings

trait ZipFileResourceImplPlatformSpecific[-R, +E >: IOException] extends ZipFileResource[R, E] {
  override def asBytes: ZStream[R, E, Byte] =
    ZStream.unwrapScoped(
      for
        jszip <- ZIO.succeed { new JSZipClass() }
        zip <- asZip
        _ <- zip.entries.foreach { entry =>
          dataStreamToUint8Array(entry.value.asBytes).flatMap { data =>
            ZIO.succeed { jszip.file(entry.path, data) }
          }
        }
        zipData <- ZIO.fromPromiseJS {
          val options = JSZipGeneratorOptions[jszipStrings.uint8array]()
          options.`type` = jszipStrings.uint8array

          jszip.generateAsync_uint8array(options)
        }.orDie
        zipDataChunk <- uint8ArrayToChunk(zipData)
      yield ZStream.fromChunk(zipDataChunk)
    )
}
