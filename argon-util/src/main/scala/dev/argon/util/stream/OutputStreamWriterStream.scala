package dev.argon.util.stream

import java.io.{IOException, OutputStream}

import cats.Monad
import cats.data.NonEmptyVector
import dev.argon.util.stream.OutputStreamWriterStream.TransformOutputStreamState
import scalaz.zio
import scalaz.zio.{IO, UIO, ZIO, stream}


final case class OutputStreamWriterStream(rt: zio.Runtime[_])(f: OutputStream => IO[IOException, Unit]) extends ArStream[ZIO, Any, IOException, Byte] {

  override def foldLeft[R2 <: Any, E2 >: IOException, A2 >: Byte, X](trans: StreamTransformation[ZIO, R2, E2, A2, Unit, Nothing, X])(implicit monadInstance: Monad[ZIO[R2, E2, ?]]): ZIO[R2, E2, X] =
    trans match {
      case trans: OutputStreamTransformation[X] => trans.writeDirectly[E2](f)
      case _ =>
        ZIO.accessM { r2 =>
          @SuppressWarnings(Array("org.wartremover.warts.Var"))
          final class TransformOutputStream(var state: TransformOutputStreamState[E2, trans.State, X]) extends OutputStream {

            private def feed(state: trans.State, data: Vector[Byte]): ZIO[R2, E2, TransformOutputStreamState[Nothing, trans.State, X]] =
              NonEmptyVector.fromVector(data) match {
                case Some(chunk) => trans.step(state, chunk).flatMap {
                  case Step.Produce(_, value, _) => value
                  case Step.Continue(state) => feed(state, data)
                  case Step.Stop(result) => IO.succeed(OutputStreamWriterStream.Stopped(result))
                }

                case None => IO.succeed(OutputStreamWriterStream.Running(state))
              }

            private def writeIO(data: Vector[Byte]): Unit =
              state match {
                case OutputStreamWriterStream.Running(s) =>
                  state = rt.unsafeRun(feed(s, data).either.map {
                    case Left(error) => OutputStreamWriterStream.Failed(error)
                    case Right(newState) => newState
                  }.provide(r2))

                case OutputStreamWriterStream.Stopped(_) | OutputStreamWriterStream.Failed(_) =>
                  throw OutputStreamWriterStream.TransformOutputStreamStopException()
              }


            override def write(b: Int): Unit = writeIO(Vector(b.toByte))

            override def write(b: Array[Byte]): Unit = writeIO(b.toVector)

            override def write(b: Array[Byte], off: Int, len: Int): Unit =
              writeIO(b.slice(off, off + len).toVector)
          }

          trans.initial
            .flatMap { init =>
              IO.effectTotal { new TransformOutputStream(OutputStreamWriterStream.Running(init)) }
            }
            .flatMap { stream =>
              f(stream)
                .catchSome {
                  case OutputStreamWriterStream.TransformOutputStreamStopException() => IO.succeed(())
                }
                .flatMap { _ => IO.effectTotal { stream.state } }
                .flatMap {
                  case OutputStreamWriterStream.Running(state) =>
                    trans.end(state, ()).flatMap { case (_, fr) => fr }
                  case OutputStreamWriterStream.Stopped(result) =>
                    IO.succeed(result)
                  case OutputStreamWriterStream.Failed(error) =>
                    IO.fail(error)
                }

            }
        }
    }

  override def toZStream(toIO: ArStream.EffectConverter[ZIO, ZIO]): stream.ZStream[Any, IOException, Byte] = ???

}

object OutputStreamWriterStream {

  private final case class TransformOutputStreamStopException() extends IOException

  private sealed trait TransformOutputStreamState[+E, +S, +R]
  private final case class Running[+S](state: S) extends TransformOutputStreamState[Nothing, S, Nothing]
  private final case class Stopped[+R](result: R) extends TransformOutputStreamState[Nothing, Nothing, R]
  private final case class Failed[+E](error: E) extends TransformOutputStreamState[E, Nothing, Nothing]

}
