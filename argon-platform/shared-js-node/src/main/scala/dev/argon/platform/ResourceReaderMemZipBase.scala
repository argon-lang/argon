package dev.argon.platform

import java.io.IOException

import dev.argon.io.{JSIOException, ZipFileReader}
import dev.argon.stream.builder.{Source, ZStreamSource}
import zio._
import zio.stream.{ZSink, ZStream}

import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.|

trait ResourceReaderMemZipBase {

  @SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
  private def promiseToIO[E, A](errorHandler: IOException => E)(promise: => js.Promise[A]): IO[E, A] =
    IO.effectAsync { register =>
      val _ = promise
        .`then`[Unit](
          onFulfilled = data => register(IO.succeed(data)),
          onRejected = {
            case e: js.Error => register(IO.fail(errorHandler(JSIOException(e))))
            case _ => register(IO.fail(errorHandler(new IOException("An unknown error occurred"))))
          } : js.Function1[Any, Unit | js.Thenable[Unit]]
        )
    }

  def zipReaderForStream[R, E](errorHandler: IOException => E)(data: ZStream[R, E, Chunk[Byte]]): ZIO[R, E, ZipFileReader[Any, E]] =
      data.fold(Chunk.empty : Chunk[Byte])(_ ++ _)
        .flatMap { data =>
          promiseToIO(errorHandler)(new JSZip().loadAsync(new Uint8Array(data.toArray.toJSArray)))
        }
        .map { zip =>
          new ZipFileReader[Any, E] {
            override def getEntryStream(name: String): Source[Any, E, Chunk[Byte], Unit] =
              new ZStreamSource(
                ZStream.flatten(
                  ZStream.fromEffect(
                    promiseToIO(errorHandler)(zip.file(name).async("uint8array"))
                      .map { data => ZStream(Chunk.fromArray(data.toArray.map { _.toByte })) }
                  )
                )
              )

          }
        }
}
