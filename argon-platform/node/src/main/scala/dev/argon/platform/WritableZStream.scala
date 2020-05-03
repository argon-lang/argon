package dev.argon.platform
import zio.Chunk

import scala.scalajs.js
import zio._
import zio.stream.ZStream.Pull
import zio.stream.{Take, ZStream}
import cats._
import cats.implicits._
import zio.interop.catz._

import scala.scalajs.js.annotation.ScalaJSDefined

private[platform] class WritableZStream[R] private(runtime: Runtime[R], queue: Queue[Take[js.Error, Chunk[Byte]]]) extends NodeWritable {

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
      queue.offer(Take.Value(bufferToChunk(buffer)))
    ) {
      case Exit.Success(_) => callback(null)
      case Exit.Failure(cause) => callback(cause)
    }

  @SuppressWarnings(Array("org.wartremover.warts.Null", "org.wartremover.warts.Equals"))
  override protected def _destroy(err: js.Error, callback: js.Function1[Any, Unit]): Unit =
    runtime.unsafeRunAsync(
      queue.offer(Take.End)
    ) {
      case Exit.Success(_) => callback(null)
      case Exit.Failure(cause) => callback(cause)
    }
}

object WritableZStream {

  @SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
  def apply[R](fill: NodeWritable => ZIO[R, js.Error, Unit]): ZStream[R, js.Error, Chunk[Byte]] =
    ZStream.unwrapManaged(
      for {
        queue <- ZManaged.make(Queue.bounded[Take[js.Error, Chunk[Byte]]](10))(_.shutdown)
        wzs <- ZManaged.fromEffect(
          for {
            runtime <- ZIO.runtime[R]
            wzs <- IO.effectTotal { new WritableZStream[R](runtime, queue) }
          } yield wzs
        )

        _ <- (fill(wzs) *> IO.effectTotal { wzs.destroy() }).forkManaged

      } yield ZStream.fromQueue(queue).unTake
    )

}
