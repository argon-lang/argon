package dev.argon.util.stream

import java.io.{IOException, OutputStream}
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.{ArrayBlockingQueue, BlockingQueue}

import cats.Monad
import cats.data.NonEmptyVector
import dev.argon.util.stream.OutputStreamWriterStream.{TransformOutputStreamState, TransformOutputStreamStopException}
import scalaz.zio
import scalaz.zio.blocking.Blocking
import scalaz.zio.stream.ZStream.Fold
import scalaz.zio.{Chunk, Exit, Fiber, IO, UIO, ZIO, stream}


final case class OutputStreamWriterStream[R, E](f: OutputStream => ZIO[R, E, Unit]) extends ArStream[ZIO, R with Blocking, E, Byte] {


  private final class TransformOutputStream(queue: BlockingQueue[Vector[Byte]]) extends OutputStream {

    val isStopped: AtomicBoolean = new AtomicBoolean()

    override def write(b: Int): Unit =
      if(isStopped.get())
        throw new TransformOutputStreamStopException()
      else
        queue.put(Vector(b.toByte))

    override def write(b: Array[Byte], off: Int, len: Int): Unit =
      if(isStopped.get())
        throw new TransformOutputStreamStopException()
      else
        queue.put(b.slice(off, off + len).toVector)

  }



  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  private def runOutputFunc[R2 <: R with Blocking, E2 >: E, X](consumer: ZIO[R2, E2, Option[NonEmptyVector[Byte]]] => ZIO[R2, E2, X]): ZIO[R2, E2, X] =
    IO.effectTotal { new ArrayBlockingQueue[Vector[Byte]](32) }.flatMap { queue =>
      IO.effectTotal { new TransformOutputStream(queue) }.flatMap { outputStream =>
        val producer = f(outputStream)
          .onTermination(_ => IO.effectTotal { queue.put(null) })
          .sandbox
          .catchSome {
            case Exit.Cause.Die(_: TransformOutputStreamStopException) => IO.succeed(())
            case Exit.Cause.Fail(_: TransformOutputStreamStopException) => IO.succeed(())
          }
          .unsandbox

        producer.fork.flatMap { producerFiber =>

          def takeNext: ZIO[R with Blocking, E, Option[NonEmptyVector[Byte]]] =
            ZIO.environment[Blocking].flatMap(_.blocking.blocking(IO.effectTotal { queue.take() })).flatMap {
              case null => producerFiber.join.const(None)
              case data => NonEmptyVector.fromVector(data) match {
                case Some(data) => IO.succeed(Some(data))
                case None => takeNext
              }
            }

          consumer(takeNext)
            .onTermination(_ => IO.effectTotal { outputStream.isStopped.set(true) })
            .flatMap { x =>
              producerFiber.join.const(x)
            }
        }
      }
    }


  override def foldLeft[R2 <: R with Blocking, E2 >: E, A2 >: Byte, X](trans: StreamTransformation[ZIO, R2, E2, A2, Unit, Nothing, X])(implicit monadInstance: Monad[ZIO[R2, E2, ?]]): ZIO[R2, E2, X] =
    trans match {
      case trans: OutputStreamTransformation[R2, E2, X] => trans.writeDirectly(f)
      case _ =>
        runOutputFunc[R2, E2, X] { takeNext =>
          trans.initial.flatMap { state =>

            def feed(state: trans.State): ZIO[R2, E2, X] =
              takeNext.flatMap {
                case Some(data) => trans.step(state, data).flatMap {
                  case Step.Produce(_, value, _) => value
                  case Step.Continue(state) => feed(state)
                  case Step.Stop(result) => IO.succeed(result)
                }

                case None =>
                  trans.end(state, ()).flatMap { case (_, fx) => fx }
              }

            feed(state)
          }
        }
    }

  override def toZStream(toIO: ArStream.EffectConverter[ZIO, ZIO]): stream.ZStream[R with Blocking, E, Byte] =
    new stream.ZStream[R with Blocking, E, Byte] {
      override def fold[R1 <: R with Blocking, E1 >: E, A2 >: Byte, S]: Fold[R1, E1, A2, S] =
        IO.succeedLazy { (s, cont, feed) =>

          runOutputFunc[R1, E1, S] { takeNext =>
            def iter(s: S, chunk: Vector[A2]): ZIO[R1, E1, S] =
              chunk match {
                case _ if !cont(s) => IO.succeed(s)
                case h +: t => feed(s, h).flatMap { s => iter(s, t) }
                case Vector() =>
                  takeNext.flatMap {
                    case Some(data) => iter(s, data.toVector)
                    case None => IO.succeed(s)
                  }
              }

            iter(s, Vector())
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
