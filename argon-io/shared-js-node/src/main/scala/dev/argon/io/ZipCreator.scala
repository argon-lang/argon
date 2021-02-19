package dev.argon.io

import zio.{Chunk, IO, ZIO}
import zio.stream.ZStream
import FileIOUtil._

object ZipCreator {

  @SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
  def zipEntries[R](entries: ZStream[R, Throwable, ZipEntryInfo[R, Throwable]]): ZStream[R, Throwable, Byte] =
    ZStream.unwrap(
      IO.effectTotal { new JSZip() }
        .flatMap { zip =>
          entries.foreach { entry =>
            dataStreamToUint8Array(entry.dataStream).flatMap { buffer =>
              IO.effect { zip.file(entry.path, buffer) }
                .orDie
                .unit
            }
          }
            .flatMap { _ =>
              ZIO.fromPromiseJS(zip.generateAsync(JSZip.JSZipGeneratorOptions("uint8array")))
                .map { data =>
                  val arr = new Array[Byte](data.length)
                  for(i <- arr.indices) {
                    arr(i) = data(i).toByte
                  }

                  ZStream.fromChunk(Chunk.fromArray(arr))
                }
            }
        }
    )
}
