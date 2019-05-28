package dev.argon.util.stream

import java.io.{IOException, InputStream}

import cats.Monad
import cats.data.NonEmptyVector
import scalaz.zio.{IO, ZIO}
import scalaz.zio.stream.ZStream

final class InputStreamStream[R, E](errorHandler: IOException => E)(inputStreamResource: Resource[ZIO, R, E, InputStream]) extends ArStream[ZIO, R, E, Byte] {

  override def foldLeft[R2 <: R, E2 >: E, A2 >: Byte, X](trans: StreamTransformation[ZIO, R2, E2, A2, Unit, Nothing, X])(implicit monadInstance: Monad[ZIO[R2, E2, ?]]): ZIO[R2, E2, X] =
    trans match {
      case trans: InputStreamReaderTransformation[R2, E2, X] =>
        inputStreamResource.use(trans.readDirectly)

      case _ =>
        inputStreamResource.use { inputStream =>
          trans.initial.use { s =>
            def feed(s: trans.State): ZIO[R2, E2, X] =
              IO.effect {
                val buffer = new Array[Byte](2049)
                val bytesRead = inputStream.read(buffer)
                buffer.take(bytesRead).toVector
              }
                .refineOrDie { case ex: IOException => errorHandler(ex) }
                .flatMap { chunk =>
                  NonEmptyVector.fromVector(chunk) match {
                    case Some(chunk) => trans.step(s, chunk).flatMap {
                      case Step.Produce(_, value, _) => value
                      case Step.Continue(s) => feed(s)
                      case Step.Stop(x) => IO.succeed(x)
                    }
                    case None => trans.end(s, ()).flatMap { case (_, fx) => fx }
                  }
                }

            feed(s)
          }
        }
    }

}
