package dev.argon.platform

import dev.argon.stream._
import java.io.IOException
import java.util.zip.{ZipEntry, ZipOutputStream}

import dev.argon.io.ZipEntryInfo
import zio.blocking.Blocking
import zio._
import zio.interop._
import zio.interop.catz.core._
import zio.stream.{ZSink, ZStream}

private[platform] object ZipEntryStreamTransformation {

  def apply[R, E](errorHandler: Throwable => Cause[E], blocking: Blocking.Service)(entries: ZStream[R, E, ZipEntryInfo[R, E]]): ZStream[R, E, Byte] =
    OutputStreamWriterStream { outputStream =>
      blocking.effectBlocking { new ZipOutputStream(outputStream) }.orDie.bracketAuto { zipStream =>
        entries.foreach { case ZipEntryInfo(path, dataStream) =>
          blocking.effectBlocking {
            val entry = new ZipEntry(path)
            zipStream.putNextEntry(entry)
            entry
          }
            .catchAll { ex => IO.halt(errorHandler(ex)) }
            .bracket(_ =>
              blocking.effectBlocking { zipStream.closeEntry() }.orDie
            ) { _ =>
              ZSink.fromOutputStream(zipStream).push.provide(Has(blocking)).use { push =>
                dataStream
                  .run(
                    ZSink.fromPush { chunkOpt: Option[Chunk[Byte]] =>
                      push(chunkOpt)
                        .provide(Has(blocking))
                    }
                      .foldM(
                        failure = ex => ZSink.halt(errorHandler(ex)),
                        success = ZSink.succeed(_)
                      )
                  )
              }
            }

        }
      }
    }

}
