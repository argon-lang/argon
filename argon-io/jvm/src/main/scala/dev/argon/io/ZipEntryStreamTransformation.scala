package dev.argon.io

import dev.argon.stream._
import java.io.IOException
import java.util.zip.{ZipEntry, ZipOutputStream}

import dev.argon.stream.builder.{Source, SourceIO}
import zio.blocking.Blocking
import zio._
import zio.interop._
import zio.interop.catz._
import zio.stream.{ZSink, ZStream}

object ZipEntryStreamTransformation {

  def apply[R, E](errorHandler: IOException => E, env: Blocking)(entries: Source[ZIO[R, E, *], ZipEntryInfo[ZIO[R, E, *]], Unit]): ZStream[R, E, Chunk[Byte]] =
    OutputStreamWriterStream { outputStream =>
      val blocking = env.get[Blocking.Service]

      blocking.effectBlocking { new ZipOutputStream(outputStream) }.orDie.bracketAuto { zipStream =>
        entries.foreach { case ZipEntryInfo(path, dataStream) =>
          blocking.effectBlocking {
            val entry = new ZipEntry(path)
            zipStream.putNextEntry(entry)
            entry
          }
            .refineOrDie { case e: IOException => errorHandler(e) }
            .bracket(_ =>
              blocking.effectBlocking { zipStream.closeEntry() }.orDie
            ) { _ =>
              SourceIO.fromSource(dataStream).toZStream
                .run(
                  ZSink.fromOutputStream(zipStream)
                    .provideSome[Any](_ => env)
                    .mapError(errorHandler)
                    .unit
                )
            }

        }
      }
    }

}
