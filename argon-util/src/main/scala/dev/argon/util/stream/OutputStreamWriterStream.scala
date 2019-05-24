package dev.argon.util.stream

import java.io.{IOException, OutputStream}

import cats.Monad
import cats.data.NonEmptyVector
import dev.argon.util.stream.OutputStreamWriterStream.TransformOutputStreamState
import scalaz.zio
import scalaz.zio.stream.ZStream.Fold
import scalaz.zio.{Chunk, IO, UIO, ZIO, stream}


final case class OutputStreamWriterStream[R, E](rt: zio.Runtime[_])(f: OutputStream => ZIO[R, E, Unit]) extends ArStream[ZIO, R, E, Byte] {

  override def foldLeft[R2 <: R, E2 >: E, A2 >: Byte, X](trans: StreamTransformation[ZIO, R2, E2, A2, Unit, Nothing, X])(implicit monadInstance: Monad[ZIO[R2, E2, ?]]): ZIO[R2, E2, X] =
    trans match {
      case trans: OutputStreamTransformation[E2, X] => trans.writeDirectly(f)
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

  override def toZStream(toIO: ArStream.EffectConverter[ZIO, ZIO]): stream.ZStream[R, E, Byte] =
    new stream.ZStream[R, E, Byte] {
      override def fold[R1 <: R, E1 >: E, A2 >: Byte, S]: Fold[R1, E1, A2, S] =
        IO.succeedLazy { (s, cont, feed) =>
          ZIO.accessM { r1 =>
            @SuppressWarnings(Array("org.wartremover.warts.Var"))
            final class TransformOutputStream(var state: Either[E1, S]) extends OutputStream {
              override def write(b: Int): Unit =
                state match {
                  case Right(s) if cont(s) =>
                    state = rt.unsafeRun(feed(s, b.toByte).either.provide(r1))

                  case _ =>
                    throw OutputStreamWriterStream.TransformOutputStreamStopException()
                }
            }

            IO.effectTotal { new TransformOutputStream(Right(s)) }
              .flatMap { stream =>
                f(stream)
                  .catchSome {
                    case OutputStreamWriterStream.TransformOutputStreamStopException() => IO.succeed(())
                  }
                  .flatMap { _ => IO.effectTotal { stream.state } }
                  .flatMap[R1, E1, S](IO.fromEither(_))
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
