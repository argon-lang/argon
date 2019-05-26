package dev.argon.util.stream

import java.io.{IOException, OutputStream}

import cats.data.NonEmptyVector
import scalaz.zio._
import scalaz.zio.blocking.Blocking

trait OutputStreamTransformation[-R, +E, +X] extends StreamTransformation[ZIO, R, E, Byte, Unit, Nothing, X] {
  def writeDirectly[R2 <: R, E2 >: E](f: OutputStream => ZIO[R2, E2, Unit]): ZIO[R2, E2, X]
}

object OutputStreamTransformation {

  def apply[E](errorHandler: IOException => E)(outputStream: OutputStream): OutputStreamTransformation[Blocking, E, Unit] = new OutputStreamTransformation[Blocking, E, Unit] {

    override type State = Unit

    override def initial: IO[E, Unit] = IO.succeed(())

    override def step(s: Unit, ca: NonEmptyVector[Byte]): ZIO[Blocking, E, Step[Unit, Byte, Nothing, Unit]] =
      ZIO.environment[Blocking].flatMap { env =>
        env.blocking.effectBlocking {
          outputStream.write(ca.toVector.toArray)
          Step.Continue(())
        }.refineOrDie {
          case ex: IOException => errorHandler(ex)
        }
      }

    override def end(s: Unit, result: Unit): IO[E, (Vector[Nothing], IO[E, Unit])] =
      IO.succeed((Vector(), IO.succeed(())))


    override def writeDirectly[R2, E2](f: OutputStream => ZIO[R2, E2, Unit]): ZIO[R2, E2, Unit] = f(outputStream)
  }

}
