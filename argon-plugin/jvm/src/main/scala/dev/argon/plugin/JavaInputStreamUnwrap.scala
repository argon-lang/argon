package dev.argon.plugin;

import java.io.{InputStream, FilterInputStream}
import zio.*
import zio.stream.*
import dev.argon.util.ErrorWrapper
import dev.argon.util.JavaExecuteIO
import java.io.IOException
import scala.reflect.TypeTest


private[plugin] object JavaInputStreamUnwrap {

  def toZStream[E >: IOException, EX <: Exception](is: => InputStream)(using ErrorWrapper[E, EX], TypeTest[Throwable, EX]): Stream[E, Byte] =
    new ZStream[Any, E, Byte](
      ZChannel.acquireReleaseWith(
        IO.attemptBlockingInterrupt {
          is
        }
          .catchAll(JavaErrorHandler.handleErrors[E, EX])
      )(is =>
        IO.attemptBlockingInterrupt {
              is.close()
            }.orDie
      ) { is =>
        lazy val pull: ZChannel[Any, Any, Any, Any, E, Chunk[Byte], Unit] =
          ZChannel.fromZIO(
            IO.attemptBlockingInterrupt {
              val buff = new Array[Byte](ZStream.DefaultChunkSize)
              val bytesRead = is.read(buff)
              Chunk.fromArray(buff).slice(0, bytesRead)
            }.catchAll(JavaErrorHandler.handleErrors[E, EX])
          ).flatMap { chunk =>
            if chunk.isEmpty then
              ZChannel.unit
            else
              ZChannel.write(chunk).pipeTo(pull)
          }

        pull
      }
    )

}
