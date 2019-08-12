package dev.argon.io

import dev.argon.stream._
import java.io.{IOException, OutputStream}

import cats.data.NonEmptyVector
import zio._
import zio.blocking.Blocking

trait OutputStreamTransformation[-R, +E, +X] extends StreamTransformation[ZIO, R, E, Byte, Unit, Nothing, X] {
  def writeDirectly[R2 <: R, E2 >: E](f: OutputStream => ZIO[R2, E2, Unit]): ZIO[R2, E2, X]
}

object OutputStreamTransformation {

  def apply[E](errorHandler: IOException => E, blocking: Blocking.Service[Any])(outputStream: OutputStream): OutputStreamTransformation[Any, E, Unit] = new OutputStreamTransformation[Any, E, Unit] {

    override type State = Unit

    override def initial: Resource[ZIO, Any, E, Unit] = Resource.pure(())

    override def step(s: Unit, ca: NonEmptyVector[Byte]): ZIO[Any, E, Step[Unit, Byte, Nothing, Unit]] =
      blocking.effectBlocking {
        outputStream.write(ca.toVector.toArray)
        Step.Continue(())
      }.refineOrDie {
        case ex: IOException => errorHandler(ex)
      }

    override def end(s: Unit, result: Unit): IO[E, (Vector[Nothing], IO[E, Unit])] =
      IO.succeed((Vector(), IO.succeed(())))


    override def writeDirectly[R2, E2](f: OutputStream => ZIO[R2, E2, Unit]): ZIO[R2, E2, Unit] = f(outputStream)
  }

}
