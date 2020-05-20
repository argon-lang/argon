package dev.argon.platform

import dev.argon.stream._
import java.io.{IOException, OutputStream}
import java.util.Objects
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.{ArrayBlockingQueue, BlockingQueue}

import cats.data.NonEmptyVector
import cats._
import cats.implicits._
import zio._
import zio.blocking.Blocking
import zio.stream.ZStream

private[platform] object OutputStreamWriterStream {

  @SuppressWarnings(Array(
    "org.wartremover.warts.NonUnitStatements",
    "org.wartremover.warts.Var",
  ))
  private final class TransformOutputStream[R, E]
  (
    runtime: Runtime[R],
    queue: Queue[Exit[Option[E], Chunk[Byte]]],
  ) extends OutputStream {

    override def write(b: Int): Unit = write(Array(b.toByte))

    override def write(b: Array[Byte], off: Int, len: Int): Unit = {
      Objects.checkFromIndexSize(off, len, b.length)
      val _ = runtime.unsafeRun(queue.offer(Exit.Success(Chunk.fromArray(b.slice(off, off + len)))))
    }
  }




  def apply[R, E](f: OutputStream => ZIO[R, E, Unit]): ZStream[R, E, Byte] =
    ZStream.unwrap(
      for {
        runtime <- ZIO.runtime[R]
        queue <- Queue.bounded[Exit[Option[E], Chunk[Byte]]](1)
        writeTask <- f(new TransformOutputStream(runtime, queue))
          .foldCauseM(
            failure = e => queue.offer(Exit.Failure(e.map(Some.apply))),
            success = _ => queue.offer(Exit.fail(None))
          )
          .fork

      } yield ZStream.fromQueueWithShutdown(queue).collectWhileSuccess.flattenChunks ++ ZStream.fromEffect(writeTask.interrupt).drain
    )


}
