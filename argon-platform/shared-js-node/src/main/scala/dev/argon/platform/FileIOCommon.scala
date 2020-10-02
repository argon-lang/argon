package dev.argon.platform

import java.io.{ByteArrayOutputStream, IOException}

import dev.argon.io.JSIOException
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import zio.{Cause, Chunk, IO, ZIO}
import zio.stream.ZStream

import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array
import scala.scalajs.js.|
import scalajs.js.JSConverters._

@SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
private[platform] trait FileIOCommon extends JSErrorHandler {
  protected def dataStreamToArray[R, E](dataStream: ZStream[R, E, Byte]): ZIO[R, E, Array[Byte]] =
    IO.effectTotal { new ByteArrayOutputStream() }
      .flatMap { outputStream =>
        dataStream.foreachChunk { chunk =>
          IO.effectTotal { outputStream.write(chunk.toArray) }
        }
          .flatMap { _ =>
            IO.effectTotal { outputStream.toByteArray }
          }
      }
  protected def dataStreamToUint8Array[R, E](dataStream: ZStream[R, E, Byte]): ZIO[R, E, Uint8Array] =
    dataStreamToArray(dataStream).map { arr =>
      val u8arr = new Uint8Array(arr.length)
      for(i <- arr.indices) {
        u8arr(i) = (arr(i) & 0xFF).toShort
      }
      u8arr
    }

  protected def promiseToIO[E, A](errorHandler: Throwable => Cause[E])(promise: => js.Promise[A]): IO[E, A] =
    IO.effectAsync { register =>
      val _ = promise
        .`then`[Unit](
          onFulfilled = data => register(IO.succeed(data)),
          onRejected = (e => register(IO.halt(errorHandler(handleJSError(e))))) : js.Function1[Any, Unit | js.Thenable[Unit]]
        )
    }
}
