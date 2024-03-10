package dev.argon.io

import zio.*
import zio.stream.*

import java.io.IOException
import dev.argon.io.jstypes.jszip.JSZip

import scala.scalajs.js

trait ZipStreamResourceImplPlatformSpecific[+E >: IOException] extends ZipStreamResource[E] {
  override def asBytes: ZStream[Any, E, Byte] =
    ZStream.unwrapScoped(
      for
        jszip <- ZIO.succeed { new JSZip() }
        zip <- asZip
        _ <- zip.entries.foreach { entry =>
          dataStreamToUint8Array(entry.value.asBytes).flatMap { data =>
            ZIO.succeed { jszip.file["uint8array"](entry.path, data) }
          }
        }
        zipData <- ZIO.fromPromiseJS {
          val options = new JSZip.JSZipGeneratorOptions["uint8array"] {
            override val `type`: js.UndefOr["uint8array"] = "uint8array"
          }

          jszip.generateAsync(options)
        }.orDie
      yield ZStream.fromChunk(uint8ArrayToChunk(zipData))
    )
}
