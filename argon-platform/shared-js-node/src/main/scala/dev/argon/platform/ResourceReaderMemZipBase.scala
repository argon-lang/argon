package dev.argon.platform

import java.io.IOException

import dev.argon.io.{JSIOException, ZipFileReader}
import zio._
import zio.stream.{ZSink, ZStream}

import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.|

@SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
trait ResourceReaderMemZipBase {

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

  def zipReaderForStream[R, E](errorHandler: IOException => E)(data: ZStream[R, E, Byte]): ZIO[R, E, ZipFileReader[Any, E]] =
      data.run(ZSink.foldLeftChunks(Chunk.empty : Chunk[Byte]) { _ ++ _ })
        .flatMap { data =>
          promiseToIO(errorHandler)(new JSZip().loadAsync {
            val u8arr = new Uint8Array(data.length)
            for(i <- data.indices) {
              u8arr(i) = (data.byte(i) & 0xFF).toShort
            }
            u8arr
          })
        }
        .map { zip =>
          new ZipFileReader[Any, E] {


            override def getEntryStream(name: String): ZIO[Any, E, Option[ZStream[Any, E, Byte]]] =
              IO.effectTotal { Option(zip.file(name)) }.flatMap { zipEntryOpt =>
                  ZIO.foreach(zipEntryOpt) { zipEntry =>
                    promiseToIO(errorHandler)(zipEntry.async("uint8array"))
                      .map { data => ZStream.fromChunk(Chunk.fromArray(data.toArray.map { _.toByte })) }
                  }
                }

          }
        }
}
