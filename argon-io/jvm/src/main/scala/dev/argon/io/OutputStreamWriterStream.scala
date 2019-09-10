package dev.argon.io

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
import zio.stream.{Take, ZStream}

object OutputStreamWriterStream {

  @SuppressWarnings(Array(
    "org.wartremover.warts.NonUnitStatements",
    "org.wartremover.warts.Var",
  ))
  private final class TransformOutputStream[R, E, A2 >: Byte, X, S]
  (
    runtime: Runtime[R],
    queue: Queue[Take[E, Chunk[Byte]]],
  ) extends OutputStream {

    var result: Option[X] = None

    override def write(b: Int): Unit = write(Array(b.toByte))

    override def write(b: Array[Byte], off: Int, len: Int): Unit = {
      Objects.checkFromIndexSize(off, len, b.length)
      val _ = runtime.unsafeRun(queue.offer(Take.Value(Chunk.fromArray(b.slice(off, off + len)))))
    }
  }




  def apply[R, E](f: OutputStream => ZIO[R, E, Unit]): ZStream[R, E, Chunk[Byte]] =
    ZStream.flatten(
      ZStream.fromEffect(
        for {
          runtime <- ZIO.runtime[R]
          queue <- Queue.bounded[Take[E, Chunk[Byte]]](1)
          writeTask <- f(new TransformOutputStream(runtime, queue))
            .foldCauseM(
              failure = e => queue.offer(Take.Fail(e)),
              success = _ => queue.offer(Take.End)
            )
            .fork

        } yield ZStream.fromQueue(queue).unTake ++ ZStream.fromEffect(writeTask.interrupt).drain
      )
    )


}
