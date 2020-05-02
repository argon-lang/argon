package dev.argon.platform

import java.io.{ByteArrayOutputStream, IOException}

import dev.argon.io.JSIOException
import dev.argon.stream.builder.{Source, SourceIO, ZStreamSource}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import zio.{Chunk, IO, ZIO}
import zio.stream.ZStream

import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array
import scala.scalajs.js.|
import scalajs.js.JSConverters._

@SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
private[platform] trait FileIOCommon {
  protected def dataStreamToArray[R, E](dataStream: Source[ZIO[R, E, *], Chunk[Byte], Unit]): ZIO[R, E, Array[Byte]] =
    IO.effectTotal { new ByteArrayOutputStream() }
      .flatMap { outputStream =>
        dataStream.foreach { chunk =>
          IO.effectTotal { outputStream.write(chunk.toArray) }
        }
          .flatMap { _ =>
            IO.effectTotal { outputStream.toByteArray }
          }
      }
  protected def dataStreamToUint8Array[R, E](dataStream: Source[ZIO[R, E, *], Chunk[Byte], Unit]): ZIO[R, E, Uint8Array] =
    dataStreamToArray(dataStream).map { arr => new Uint8Array(arr.toJSArray) }

  protected def promiseToIO[E, A](errorHandler: IOException => E)(promise: => js.Promise[A]): IO[E, A] =
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
}
