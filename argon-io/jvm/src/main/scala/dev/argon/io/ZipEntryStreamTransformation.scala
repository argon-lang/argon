package dev.argon.io

import dev.argon.stream._
import java.io.IOException
import java.util.zip.{ZipEntry, ZipOutputStream}

import zio.blocking.Blocking
import zio._
import zio.interop._
import zio.interop.catz._

object ZipEntryStreamTransformation {

  def apply[R, E](errorHandler: IOException => E, blocking: Blocking.Service[Any])(stream: ArStream[ZIO, R, E, ZipEntryInfo[ZIO, R, E]]): ArStream[ZIO, R, E, Byte] =
    OutputStreamWriterStream(blocking) { outputStream =>
      blocking.effectBlocking { new ZipOutputStream(outputStream) }.orDie.bracketAuto { zipStream =>
        stream.forEach { case ZipEntryInfo(path, dataStream) =>
          blocking.effectBlocking {
            val entry = new ZipEntry(path)
            zipStream.putNextEntry(entry)
            entry
          }
            .refineOrDie { case e: IOException => errorHandler(e) }
            .bracket(_ =>
              blocking.blocking(IO.effectTotal { zipStream.closeEntry() })
            ) { _ =>
              dataStream.foldLeft(OutputStreamTransformation(errorHandler, blocking)(zipStream))
            }

        }
      }
    }

}
