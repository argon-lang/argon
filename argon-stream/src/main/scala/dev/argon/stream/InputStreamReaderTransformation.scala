package dev.argon.stream

import java.io.InputStream
import java.util.Objects
import java.util.concurrent.{ArrayBlockingQueue, BlockingQueue}

import cats.data.NonEmptyVector
import scalaz.zio.blocking.Blocking
import scalaz.zio._

trait InputStreamReaderTransformation[-R, +E, +X] extends StreamTransformation[ZIO, R, E, Byte, Unit, Nothing, X] {

  def readDirectly[R2 <: R, E2 >: E](inputStream: InputStream): ZIO[R2, E2, X]

}

object InputStreamReaderTransformation {

  @SuppressWarnings(Array(
    "org.wartremover.warts.Equals",
    "org.wartremover.warts.NonUnitStatements",
    "org.wartremover.warts.Var",
  ))
  private final class TransformInputStream(queue: BlockingQueue[Option[Vector[Byte]]]) extends InputStream {

    private var remainingData: Option[Vector[Byte]] = None

    override def read(): Int = {
      val buff = new Array[Byte](1)
      if(read(buff) < 0)
        -1
      else
        buff(0).toInt
    }

    override def read(b: Array[Byte], off: Int, len: Int): Int = {
      Objects.checkFromIndexSize(off, len, b.length)
      if(len == 0)
        0
      else {
        (if (remainingData.isEmpty) queue.take() else remainingData) match {
          case None => -1
          case Some(data) =>
            data.copyToArray(b, off, len)

            remainingData = if(len >= data.size) None else Some(data.drop(len))
            Math.min(len, data.size)
        }
      }
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  def apply[R <: Blocking, E, X](readHandler: InputStream => ZIO[R, E, X]): InputStreamReaderTransformation[R, E, X] =
    new InputStreamReaderTransformation[R, E, X] {
      override def readDirectly[R2 <: R, E2 >: E](inputStream: InputStream): ZIO[R2, E2, X] =
        readHandler(inputStream)

      override type State = (BlockingQueue[Option[Vector[Byte]]], Ref[Boolean], Fiber[E, X])

      override def initial: Resource[ZIO, R, E, State] =
        Resource.fromZManaged(ZManaged.make(
          for {
            env <- ZIO.environment[Blocking]
            queue <- IO.effectTotal { new ArrayBlockingQueue[Option[Vector[Byte]]](32) }
            inputStream <- IO.effectTotal { new TransformInputStream(queue) }

            doneReading <- Ref.make(false)

            readerTask <- readHandler(inputStream)
              .onTermination { _ =>
                doneReading.set(true).flatMap { _ =>
                  ZIO.effectTotal { queue.poll() }
                }
              }
              .fork

          } yield (queue, doneReading, readerTask)

        ) {
          case (queue, doneReading, _) =>
            ZIO.environment[Blocking].flatMap { env =>
              doneReading.get.flatMap {
                case true => IO.succeed(())
                case false => IO.effectTotal { queue.put(None) }
              }
            }
        })

      override def step(s: State, ca: NonEmptyVector[Byte]): ZIO[R, E, Step[State, Byte, Nothing, X]] =
        s._2.get.flatMap {
          case true => s._3.join.map(Step.Stop.apply)
          case false => IO.effectTotal {
            s._1.put(Some(ca.toVector))
            Step.Continue(s)
          }
        }

      override def end(s: State, result: Unit): ZIO[R, E, (Vector[Nothing], ZIO[R, E, X])] =
        IO.succeed((Vector(), s._3.join))
    }

}


