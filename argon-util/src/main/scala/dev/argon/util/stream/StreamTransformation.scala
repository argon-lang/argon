package dev.argon.util.stream

import cats._
import cats.data._
import cats.implicits._
import scalaz.zio._
import scalaz.zio.stream.ZSink

trait StreamTransformation[+F[-_, +_, +_], -R, +E, A, -X, +B, +Y] {

  type State

  def initial: F[R, E, State]

  def step(s: State, ca: NonEmptyVector[A]): F[R, E, Step[State, A, B, Y]]

  def end(s: State, result: X): F[R, E, (Vector[B], F[R, E, Y])]
}

object StreamTransformation {

  abstract class Single[F[-_, +_, +_], R, E, A, -X, +B, +Y](implicit monad: Monad[F[R, E, ?]]) extends StreamTransformation[F, R, E, A, X, B, Y] {

    def stepSingle(s: State, a: A): F[R, E, Step[State, A, B, Y]]

    override def step(s: State, ca: NonEmptyVector[A]): F[R, E, Step[State, A, B, Y]] =
      stepSingle(s, ca.head).flatMap {
        case Step.Produce(s, value, remaining) => Step.Produce(s, value, remaining ++ ca.tail).pure[F[R, E, ?]]
        case Step.Continue(s) =>
          NonEmptyVector.fromVector(ca.tail) match {
            case Some(tail) => step(s, tail)
            case None => Step.Continue(s).pure[F[R, E, ?]]
          }

        case stop @ Step.Stop(_) => stop.pure[F[R, E, ?]]
      }
  }

  trait Pure[+E, A, -X, +B, +Y] extends StreamTransformation[PureEffect, Any, E, A, X, B, Y] {

    type State

    def initialPure: State

    def stepPure(s: State, ca: NonEmptyVector[A]): StepPure[State, E, A, B, Y]

    def endPure(s: State, result: X): (Vector[B], Either[E, Y])

    final override def initial: PureEffect[Any, E, State] = PureEffect(initialPure)

    override def step(s: State, ca: NonEmptyVector[A]): PureEffect[Any, E, Step[State, A, B, Y]] =
      stepPure(s, ca) match {
        case value: Step[State, A, B, Y] => PureEffect(value)
        case Step.Fail(error) => PureEffect.fail(error)
      }

    override def end(s: State, result: X): PureEffect[Any, E, (Vector[B], PureEffect[Any, E, Y])] = {
      val (lastB, r2) = endPure(s, result)
      PureEffect((lastB, PureEffect.fromEither(r2)))
    }

  }

  trait PureSingle[E, A, -X, +B, +Y] extends Single[PureEffect, Any, E, A, X, B, Y] with Pure[E, A, X, B, Y] {

    def stepSinglePure(s: State, a: A): StepPure[State, E, A, B, Y]

    final override def stepPure(s: State, ca: NonEmptyVector[A]): StepPure[State, E, A, B, Y] =
      stepSinglePure(s, ca.head) match {
        case Step.Produce(s, value, remaining) => Step.Produce(s, value, remaining ++ ca.tail)
        case Step.Continue(s) =>
          NonEmptyVector.fromVector(ca.tail) match {
            case Some(tail) => stepPure(s, tail)
            case None => Step.Continue(s)
          }
        case fail @ Step.Fail(_) => fail
        case stop @ Step.Stop(_) => stop
      }

    final override def stepSingle(s: State, a: A): PureEffect[Any, E, Step[State, A, B, Y]] =
      stepSinglePure(s, a) match {
        case step: Step[State, A, B, Y] => PureEffect(step)
        case Step.Fail(error) => PureEffect.fail(error)
      }

  }

  def identity[F[-_, +_, +_], R, E, A, X](implicit applicative: Applicative[F[R, E, ?]]): StreamTransformation[F, R, E, A, X, A, X] = new StreamTransformation[F, R, E, A, X, A, X] {
    override type State = Unit

    type FE[+Y] = F[R, E, Y]

    override def initial: FE[Unit] = ().pure[FE]

    override def step(s: Unit, ca: NonEmptyVector[A]): FE[Step[Unit, A, A, X]] =
      Step.Produce((), ca.head, ca.tail).pure[FE]

    override def end(s: Unit, result: X): FE[(Vector[A], FE[X])] =
      (Vector.empty, result.pure[FE]).pure[FE]

  }

  def flattenVector[F[-_, +_, +_], R, E, A, X](implicit monad: Monad[F[R, E, ?]]): StreamTransformation[F, R, E, Vector[A], X, A, X] = new StreamTransformation.Single[F, R, E, Vector[A], X, A, X] {
    override type State = Unit

    override def initial: F[R, E, State] = ().pure[F[R, E, ?]]

    override def stepSingle(s: State, a: Vector[A]): F[R, E, Step[State, Vector[A], A, X]] =
      a match {
        case head +: tail => Step.Produce((), head, Vector(tail)).pure[F[R, E, ?]]
        case Vector() => Step.Continue(()).pure[F[R, E, ?]]
      }


    override def end(s: State, result: X): F[R, E, (Vector[A], F[R, E, X])] =
      (Vector.empty, result.pure[F[R, E, ?]]).pure[F[R, E, ?]]
  }


  def toVector[F[-_, +_, +_], R, E, A](implicit monad: Monad[F[R, E, ?]]): StreamTransformation[F, R, E, A, Unit, Nothing, Vector[A]] = new StreamTransformation[F, R, E, A, Unit, Nothing, Vector[A]] {
    override type State = Vector[A]

    override def initial: F[R, E, State] = Vector().pure[F[R, E, ?]]

    override def step(s: State, ca: NonEmptyVector[A]): F[R, E, Step[State, A, Nothing, Vector[A]]] =
      Step.Continue(s ++ ca.toVector).pure[F[R, E, ?]]

    override def end(s: Vector[A], result: Unit): F[R, E, (Vector[Nothing], F[R, E, Vector[A]])] =
      (Vector.empty, s.pure[F[R, E, ?]]).pure[F[R, E, ?]]
  }

  def fromZSink[R, E, A, B](zsink: ZSink[R, E, A, A, B]): StreamTransformation[ZIO, R, E, A, Unit, Nothing, B] = new StreamTransformation[ZIO, R, E, A, Unit, Nothing, B] {
    override type State = zsink.State

    override def initial: ZIO[R, E, zsink.State] =
      zsink.initial.map(ZSink.Step.state)

    private def feed(s: zsink.State, ca: Chunk[A]): ZIO[R, E, Step[zsink.State, A, Nothing, B]] =
      zsink.stepChunk(s, ca).flatMap { step =>
        if(ZSink.Step.cont(step))
          ZSink.Step.leftover(step) match {
            case ca if ca.notEmpty => feed(ZSink.Step.state(step), ca)
            case _ => IO.succeed(Step.Continue(ZSink.Step.state(step)))
          }
        else
          zsink.extract(ZSink.Step.state(step)).map(Step.Stop.apply)
      }


    override def step(s: zsink.State, ca: NonEmptyVector[A]): ZIO[R, E, Step[zsink.State, A, Nothing, B]] =
      feed(s, Chunk(ca.toVector: _*))

    override def end(s: zsink.State, result: Unit): ZIO[R, E, (Vector[Nothing], ZIO[R, E, B])] =
      IO.succeed((Vector(), zsink.extract(s)))
  }

}
