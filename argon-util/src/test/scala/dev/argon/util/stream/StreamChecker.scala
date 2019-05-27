package dev.argon.util.stream

import cats._
import cats.implicits._
import scalaz.zio.{stream => zstream, _}
import scalaz.zio.interop.catz._
import scalaz.zio.stream.ZSink

trait StreamChecker {

  def checkStream[F[-_, +_, +_], A: Eq](values: Vector[A], stream: ArStream[F, Any, Int, A])(implicit monadInstance: MonadError[F[Any, Int, ?], Int]): F[Any, Int, Unit] =
    stream.foldLeft(new StreamTransformation.Single[F, Any, Int, A, Unit, Nothing, Unit] {
      override type State = (Int, Vector[A])

      override def initial: Resource[F, Any, Int, State] = Resource.pure((0, values))

      override def stepSingle(s: State, a: A): F[Any, Int, Step[State, A, Nothing, Unit]] =
        s match {
          case (i, h +: _) if h =!= a => MonadError[F[Any, Int, ?], Int].raiseError(i)
          case (i, _ +: t) => Step.Continue((i + 1, t)).pure[F[Any, Int, ?]]
          case (i, Vector()) => MonadError[F[Any, Int, ?], Int].raiseError(i)
        }

      override def end(s: (Int, Vector[A]), result: Unit): F[Any, Int, (Vector[Nothing], F[Any, Int, Unit])] =
        s match {
          case (i, _ +: _) => MonadError[F[Any, Int, ?], Int].raiseError(i)
          case (_, Vector()) => (Vector(), ().pure[F[Any, Int, ?]]).pure[F[Any, Int, ?]]
        }
    })


  def checkZStream[A: Eq](values: Vector[A], stream: zstream.Stream[Int, A]): IO[Int, Unit] =
    stream.run(new ZSink[Any, Int, A, A, Unit] {
      override type State = (Int, Vector[A])

      override def initial: ZIO[Any, Int, ZSink.Step[(Int, Vector[A]), Nothing]] =
        IO.succeed(ZSink.Step.more((0, values)))

      override def step(state: (Int, Vector[A]), a: A): ZIO[Any, Int, ZSink.Step[(Int, Vector[A]), A]] =
        state match {
          case (i, h +: _) if h =!= a => IO.fail(i)
          case (i, _ +: t) => IO.succeed(ZSink.Step.more((i + 1, t)))
          case (i, Vector()) => IO.fail(i)
        }

      override def extract(state: (Int, Vector[A])): ZIO[Any, Int, Unit] =
        state match {
          case (i, _ +: _) => IO.fail(i)
          case (_, Vector()) => IO.succeed(())
        }
    })

}
