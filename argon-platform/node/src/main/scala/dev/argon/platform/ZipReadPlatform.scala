package dev.argon.platform

import dev.argon.io.ZipFileReader
import dev.argon.io.fileio.ZipRead
import zio._
import zio.stream._
import dev.argon.util.JSErrorHandler._

object ZipReadPlatform {

  @SuppressWarnings(Array("scalafix:DisableSyntax.==", "scalafix:DisableSyntax.null", "dev.argon.warts.ZioEffect"))
  def live: ULayer[ZipRead] =
    ZLayer.succeed[ZipRead.Service](new ZipRead.Service {
      override def openZipFile(path: String): Managed[Throwable, ZipFileReader[Throwable]] =
        Managed.make(
          IO.effectAsync[Throwable, NodeStreamZip] { register =>
            val zip = new NodeStreamZip(NodeStreamZip.Options(path))
            zip.on("ready", () => register(IO.succeed(zip)))
            zip.on("error", err => register(IO.fail(handleJSError(err))))
          }
        ) { zip =>
          IO.effectTotal { zip.close() }
        }
          .map { zip =>
            new ZipFileReader[Throwable] {
              override def getEntryStream(name: String): IO[Throwable, Option[Stream[Throwable, Byte]]] =
                IO.effectAsync[Throwable, Option[NodeReadable]] { register =>
                  zip.stream(name, (err, stream) => register(
                    if(err == null)
                      IO.some(stream)
                    else if(err == "Entry not found" || err == "Entry is not file")
                      IO.none
                    else
                      IO.fail(handleJSError(err))
                  ))
                }
                  .map { _.map { stream =>
                    WritableZStream { wzs =>
                      IO.effectAsync[Any, Unit] { register =>
                        stream.pipe(wzs)
                        stream.on("end", () => register(IO.succeed(())))
                      }
                    }
                      .catchAll { err => Stream.fail(handleJSError(err)) }
                  } }

            }
          }

    })
}
