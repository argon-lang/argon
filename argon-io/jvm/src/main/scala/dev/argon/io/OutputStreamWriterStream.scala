package dev.argon.io

import dev.argon.stream._
import java.io.{IOException, OutputStream}
import java.util.Objects
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.{ArrayBlockingQueue, BlockingQueue}

import cats.data.NonEmptyVector
import cats._
import cats.implicits._
import dev.argon.io.OutputStreamWriterStream.TransformOutputStreamStopException
import zio._
import zio.blocking.Blocking

final case class OutputStreamWriterStream[R, E](blocking: Blocking.Service[Any])(f: OutputStream => ZIO[R, E, Unit]) extends ArStream[ZIO, R, E, Byte] {


  @SuppressWarnings(Array(
    "org.wartremover.warts.NonUnitStatements",
    "org.wartremover.warts.Var",
  ))
  private final class TransformOutputStream[R2 <: R, E2 >: E, A2 >: Byte, X, S]
  (
    runtime: Runtime[R2],
    trans: StreamTransformation[ZIO, R2, E2, A2, Unit, Nothing, X] { type State = S },
    var state: S,
  ) extends OutputStream {

    var result: Option[X] = None

    override def write(b: Int): Unit = write(Array(b.toByte))

    override def write(b: Array[Byte], off: Int, len: Int): Unit = {
      Objects.checkFromIndexSize(off, len, b.length)

      if(result.isDefined) throw new IOException("Consumer does not expect any more data.")

      NonEmptyVector.fromVector(b.slice(off, off + len).toVector).foreach { data =>
        runtime.unsafeRun(trans.step(state, data)) match {
          case Step.Produce(_, value, _) => value
          case Step.Continue(state) => this.state = state
          case Step.Stop(result) => this.result = Some(result)
        }
      }
    }
  }


  override def foldLeft[R2 <: R, E2 >: E, A2 >: Byte, X](trans: StreamTransformation[ZIO, R2, E2, A2, Unit, Nothing, X])(implicit monadInstance: Monad[ZIO[R2, E2, ?]]): ZIO[R2, E2, X] =
    trans match {
      case trans: OutputStreamTransformation[R2, E2, X] => trans.writeDirectly(f)
      case _ =>
        trans.initial.use { state =>
          ZIO.runtime[R2].flatMap { runtime => IO.effectTotal { new TransformOutputStream[R2, E2, A2, X, trans.State](runtime, trans, state) } }
            .flatMap { outputStream =>
              f(outputStream).flatMap { _ =>
                outputStream.result match {
                  case Some(result) => IO.succeed(result)
                  case None =>
                    trans.end(outputStream.state, ())
                      .flatMap { case (_, resultIO) => resultIO }
                }
              }
            }
        }
    }

}

object OutputStreamWriterStream {

  private final case class TransformOutputStreamStopException() extends IOException

  private sealed trait TransformOutputStreamState[+E, +S, +R]
  private final case class Running[+S](state: S) extends TransformOutputStreamState[Nothing, S, Nothing]
  private final case class Stopped[+R](result: R) extends TransformOutputStreamState[Nothing, Nothing, R]
  private final case class Failed[+E](error: E) extends TransformOutputStreamState[E, Nothing, Nothing]


}
