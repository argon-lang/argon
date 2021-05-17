package dev.argon.io

import java.io.OutputStream
import java.util.Objects

import zio._
import zio.stream.ZStream

private[io] object OutputStreamWriterStream {

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

      } yield ZStream.fromQueueWithShutdown(queue).flattenExitOption.flattenChunks ++ ZStream.fromEffect(writeTask.interrupt).drain
    )


}
