package dev.argon.io

import java.util.zip.{ZipEntry, ZipOutputStream}

import zio._
import zio.blocking.Blocking
import zio.stream.{ZSink, ZStream}

private[io] object ZipEntryStreamTransformation {

  def apply[R <: Blocking](entries: ZStream[R, Throwable, ZipEntryInfo[R, Throwable]]): ZStream[R, Throwable, Byte] =
    OutputStreamWriterStream { outputStream =>
      blocking.effectBlockingInterrupt { new ZipOutputStream(outputStream) }.orDie.bracketAuto { zipStream =>
        entries.foreach { case ZipEntryInfo(path, dataStream) =>
          blocking.effectBlockingInterrupt {
            val entry = new ZipEntry(path)
            zipStream.putNextEntry(entry)
            entry
          }
            .bracket(_ =>
              blocking.effectBlockingInterrupt { zipStream.closeEntry() }.orDie
            ) { _ =>
              ZSink.fromOutputStream(zipStream).push.use { push =>
                dataStream
                  .run(
                    ZSink.fromPush { chunkOpt: Option[Chunk[Byte]] =>
                      push(chunkOpt)
                    }
                  )
              }
            }

        }
      }
    }

}
