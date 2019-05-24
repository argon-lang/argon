package dev.argon.util.stream

import java.io.{IOException, OutputStream}

import cats.data.NonEmptyVector
import scalaz.zio._

trait OutputStreamTransformation[+R] extends StreamTransformation[ZIO, Any, IOException, Byte, Unit, Nothing, R] {
  def writeDirectly[E2 >: IOException](f: OutputStream => IO[E2, Unit]): IO[E2, R]
}

object OutputStreamTransformation {

  def apply(outputStream: OutputStream): OutputStreamTransformation[Unit] = new OutputStreamTransformation[Unit] {

    override type State = Unit

    override def initial: IO[IOException, Unit] = IO.succeed(())

    override def step(s: Unit, ca: NonEmptyVector[Byte]): IO[IOException, Step[Unit, Byte, Nothing, Unit]] =
      IO.effect {
        outputStream.write(ca.toVector.toArray)
        Step.Continue(())
      }.refineOrDie {
        case ex: IOException => ex
      }

    override def end(s: Unit, result: Unit): IO[IOException, (Vector[Nothing], IO[IOException, Unit])] =
      IO.succeed((Vector(), IO.succeed(())))

    override def writeDirectly[E2 >: IOException](f: OutputStream => IO[E2, Unit]): IO[E2, Unit] = f(outputStream)
  }

}
