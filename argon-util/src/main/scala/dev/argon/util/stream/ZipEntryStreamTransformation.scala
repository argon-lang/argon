package dev.argon.util.stream

import java.io.IOException
import java.util.zip.{ZipEntry, ZipOutputStream}

import cats.Monad
import dev.argon.util.FileOperations
import scalaz.zio
import scalaz.zio._
import scalaz.zio.interop._
import scalaz.zio.interop.catz._

object ZipEntryStreamTransformation {

  def apply[R, E](errorHandler: IOException => E)(stream: ArStream[ZIO, R, E, ZipEntryInfo[ZIO, Any, E]]): ArStream[ZIO, R, E, Byte] =
    OutputStreamWriterStream { outputStream =>
      IO.effectTotal { new ZipOutputStream(outputStream) }.bracketAuto { zipStream =>
        stream.forEach { case ZipEntryInfo(path, dataStream) =>
          IO.effect {
            val entry = new ZipEntry(path)
            zipStream.putNextEntry(entry)
            entry
          }
            .refineOrDie { case e: IOException => errorHandler(e) }
            .bracket(_ =>
              IO.effectTotal { zipStream.closeEntry() }
            ) { _ =>
              dataStream.foldLeft(OutputStreamTransformation(errorHandler)(zipStream))
            }

        }
      }
    }

}
