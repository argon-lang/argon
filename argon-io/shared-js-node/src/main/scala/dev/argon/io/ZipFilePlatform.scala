package dev.argon.io

import zio.stream.*
import zio.*
import java.io.IOException

private[io] object ZipFilePlatform {

  def serializeZipFile[R](zipFile: ZipFile[R, IOException]): ZStream[R, IOException, Byte] =
    ZStream.unwrap(
      IO.effectTotal { new JSZip() }
        .flatMap { zip =>
          zipFile.entries
            .foreach { entry =>
              dataStreamToUint8Array(entry.read).flatMap { buffer =>
                IO.attempt { zip.file(entry.path, buffer) }
                  .refineToOrDie[IOException]
                  .unit
              }
            }
            .flatMap { _ =>
              ZIO.fromPromiseJS(zip.generateAsync(JSZip.JSZipGeneratorOptions("uint8array")))
                .refineToOrDie[IOException]
                .map { data =>
                  val builder = new ChunkBuilder.Byte()
                  builder.sizeHint(data.length)
                  data.foreach {
                    builder += _.toByte
                  }

                  ZStream.fromChunk(builder.result)
                }
            }
        }
    )

}
