package dev.argon.io
import zio.Chunk

import scala.scalajs.js
import zio._
import zio.stream.ZStream.Pull
import zio.stream.{Take, ZStream}
import cats._
import cats.implicits._
import zio.interop.catz._

import scala.scalajs.js.annotation.ScalaJSDefined

private[io] class WritableZStream[R] private(runtime: Runtime[R], queue: Queue[Promise[Option[js.Error], Chunk[Byte]]]) extends NodeWritable {

  private def bufferToChunk(buffer: NodeBuffer): Chunk[Byte] = {
    val array = new Array[Byte](buffer.length)
    for(i <- array.indices) {
      array(i) = buffer(i).toByte
    }
    Chunk.fromArray(array)
  }

  @SuppressWarnings(Array("org.wartremover.warts.Null", "dev.argon.warts.ZioEffect"))
  override protected def _write(buffer: NodeBuffer, encoding: String, callback: js.Function1[Any, Unit]): Unit =
    runtime.unsafeRunAsync(
      for {
        promise <- queue.take
        _ <- promise.complete(IO.effectTotal { bufferToChunk(buffer) })
      } yield ()
    ) {
      case Exit.Success(_) => callback(null)
      case Exit.Failure(cause) => callback(cause)
    }

  @SuppressWarnings(Array("org.wartremover.warts.Null", "org.wartremover.warts.Equals"))
  override protected def _destroy(err: js.Error, callback: js.Function1[Any, Unit]): Unit =
    runtime.unsafeRunAsync(
      for {
        promise <- queue.take
        _ <- promise.complete(IO.fail(
          Option.when(err != null)(err)
        ))
      } yield ()
    ) {
      case Exit.Success(_) => callback(null)
      case Exit.Failure(cause) => callback(cause)
    }

  private def getNext: Pull[Any, js.Error, Chunk[Byte]] = for {
    promise <- Promise.make[Option[js.Error], Chunk[Byte]]
    _ <- queue.offer(promise)
    data <- promise.await
  } yield data

  private def close: UIO[Unit] =
    queue.shutdown

}

object WritableZStream {

  @SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
  def apply[R](fill: NodeWritable => ZIO[R, js.Error, Unit]): ZStream[R, js.Error, Chunk[Byte]] =
    ZStream(
      ZManaged.make(
        for {
          runtime <- ZIO.runtime[R]
          queue <- Queue.bounded[Promise[Option[js.Error], Chunk[Byte]]](1)
          wzs <- IO.effectTotal { new WritableZStream[R](runtime, queue) }
          _ <- (
            fill(wzs).foldCauseM(
              failure = cause => cause.failureOrCause match {
                case Left(failure) => IO.effectTotal { wzs.destroy(failure) }
                case Right(cause) => for {
                  promise <- queue.take
                  dummyPromise <- Promise.make[Option[js.Error], Chunk[Byte]]
                  _ <- queue.offer(dummyPromise)
                  _ <- IO.effectTotal { wzs.destroy() }
                  _ <- promise.complete(IO.halt(cause))
                } yield ()
              },
              success = _ => IO.effectTotal { wzs.destroy() }
            )
          ).fork
        } yield wzs
      )(wzs => wzs.close)
        .map { wzs =>
          wzs.getNext
        }
    )

}
