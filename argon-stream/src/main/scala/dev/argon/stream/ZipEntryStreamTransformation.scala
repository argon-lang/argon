package dev.argon.stream

import java.io.IOException
import java.util.zip.{ZipEntry, ZipOutputStream}

import scalaz.zio.blocking.Blocking
import scalaz.zio._
import scalaz.zio.interop._
import scalaz.zio.interop.catz._

object ZipEntryStreamTransformation {

  def apply[R, E](errorHandler: IOException => E)(stream: ArStream[ZIO, R, E, ZipEntryInfo[ZIO, R, E]]): ArStream[ZIO, R with Blocking, E, Byte] =
    OutputStreamWriterStream { outputStream =>
      ZIO.environment[Blocking].flatMap { env =>
        env.blocking.blocking(IO.effectTotal { new ZipOutputStream(outputStream) }).bracketAuto { zipStream =>
          stream.forEach { case ZipEntryInfo(path, dataStream) =>
            env.blocking.effectBlocking {
              val entry = new ZipEntry(path)
              zipStream.putNextEntry(entry)
              entry
            }
              .refineOrDie { case e: IOException => errorHandler(e) }
              .bracket(_ =>
                env.blocking.blocking(IO.effectTotal { zipStream.closeEntry() })
              ) { _ =>
                dataStream.foldLeft(OutputStreamTransformation(errorHandler)(zipStream))
              }

          }
        }
      }
    }

}
