package dev.argon.io

import zio.stream._
import zio._

private[io] object ZipFilePlatform {

  def serializeZipFile(zipFile: ZipFile): UStream[Byte] =
    ZStream.unwrap(
      IO.effectTotal { new JSZip() }
        .flatMap { zip =>
          zipFile.entries
            .foreach { entry =>
              dataStreamToUint8Array(entry.read).flatMap { buffer =>
                IO.attempt { zip.file(entry.path, buffer) }
                  .orDie
                  .unit
              }
            }
            .flatMap { _ =>
              ZIO.fromPromiseJS(zip.generateAsync(JSZip.JSZipGeneratorOptions("uint8array")))
                .orDie
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
