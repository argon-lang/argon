package dev.argon.platform
import zio.Chunk

import scala.scalajs.js
import zio._
import zio.stream.ZStream

private[platform] class WritableZStream[R] private(runtime: Runtime[R], queue: Queue[Exit[Option[js.Error], Chunk[Byte]]]) extends NodeWritable {
  {
    val _ = js.constructorOf[NodeWritable]
  }

  private def bufferToChunk(buffer: NodeBuffer): Chunk[Byte] = {
    val array = new Array[Byte](buffer.length)
    for(i <- array.indices) {
      array(i) = buffer(i).toByte
    }
    Chunk.fromArray(array)
  }

  @SuppressWarnings(Array("scalafix:DisableSyntax.null", "dev.argon.warts.ZioEffect"))
  override protected def _write(buffer: NodeBuffer, encoding: String, callback: js.Function1[Any, Unit]): Unit =
    runtime.unsafeRunAsync(
      queue.offer(Exit.Success(bufferToChunk(buffer)))
    ) {
      case Exit.Success(_) => callback(null)
      case Exit.Failure(cause) => callback(cause)
    }

  @SuppressWarnings(Array("scalafix:DisableSyntax.null"))
  override protected def _destroy(err: Any, callback: js.Function1[Any, Unit]): Unit =
    runtime.unsafeRunAsync(
      queue.offer(Exit.fail(None))
    ) {
      case Exit.Success(_) => callback(null)
      case Exit.Failure(cause) => callback(cause)
    }
}

object WritableZStream {

  @SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
  def apply[R](fill: NodeWritable => ZIO[R, Any, Unit]): ZStream[R, Any, Byte] =
    ZStream.unwrapManaged(
      for {
        queue <- ZManaged.make(Queue.bounded[Exit[Option[js.Error], Chunk[Byte]]](10))(_.shutdown)
        wzs <- ZManaged.fromEffect(
          for {
            runtime <- ZIO.runtime[R]
            wzs <- IO.effectTotal { new WritableZStream[R](runtime, queue) }
          } yield wzs
        )

        _ <- (fill(wzs) *> IO.effectTotal { wzs.destroy() }).forkManaged

      } yield ZStream.fromQueueWithShutdown(queue).flattenExitOption.flattenChunks
    )

}
