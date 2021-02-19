package dev.argon.platform

import java.io.File
import java.util.zip.ZipFile

import dev.argon.io.ZipFileReader
import dev.argon.io.fileio.ZipRead
import zio.blocking.Blocking
import zio.stream.{Stream, ZStream}
import zio._

object ZipReadPlatform {
  def live: URLayer[Blocking, ZipRead] = ZLayer.fromFunction { env =>
    val blocking = env.get

    new ZipRead.Service {
      override def openZipFile(path: String): Managed[Throwable, ZipFileReader[Throwable]] =
        ZManaged.fromAutoCloseable(
          blocking.effectBlockingInterrupt {
            new ZipFile(new File(path))
          }
        )
          .map { zipFile =>
            new ZipFileReader[Throwable] {
              override def getEntryStream(name: String): IO[Throwable, Option[Stream[Throwable, Byte]]] =
                blocking.effectBlockingInterrupt {
                  Option(zipFile.getEntry(name))
                }
                  .map {
                    _.map { entry =>
                      ZStream.unwrapManaged(
                        ZManaged.fromAutoCloseable(
                          blocking.effectBlockingInterrupt {
                            zipFile.getInputStream(entry)
                          }
                        )
                          .map(ZStream.fromInputStream(_).provide(env))
                      )
                    }
                  }
            }
          }
    }
  }
}
