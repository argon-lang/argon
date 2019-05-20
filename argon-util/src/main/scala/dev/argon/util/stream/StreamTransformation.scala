package dev.argon.util.stream

import cats._
import cats.data._
import cats.implicits._

trait StreamTransformation[+F[+_, +_], +E, A, -R, +B, +R2] {

  type State

  def initial: F[E, State]

  def step(s: State, ca: NonEmptyVector[A]): F[E, Step[State, A, B, R2]]

  def end(s: State, result: R): F[E, (Vector[B], F[E, R2])]
}

object StreamTransformation {

  abstract class Single[F[+_, +_], E, A, -R, +B, +R2](implicit monad: Monad[F[E, ?]]) extends StreamTransformation[F, E, A, R, B, R2] {

    def stepSingle(s: State, a: A): F[E, Step[State, A, B, R2]]

    override def step(s: State, ca: NonEmptyVector[A]): F[E, Step[State, A, B, R2]] =
      stepSingle(s, ca.head).flatMap {
        case Step.Produce(s, value, remaining) => Step.Produce(s, value, remaining ++ ca.tail).pure[F[E, ?]]
        case Step.Continue(s) =>
          NonEmptyVector.fromVector(ca.tail) match {
            case Some(tail) => step(s, tail)
            case None => Step.Continue(s).pure[F[E, ?]]
          }

        case stop @ Step.Stop(_) => stop.pure[F[E, ?]]
      }
  }

  trait Pure[+E, A, -R, +B, +R2] extends StreamTransformation[Either, E, A, R, B, R2] {

    type State

    def initialPure: State

    def stepPure(s: State, ca: NonEmptyVector[A]): StepPure[State, E, A, B, R2]

    def endPure(s: State, result: R): (Vector[B], Either[E, R2])

    final override def initial: Either[E, State] = Right(initialPure)

    override def step(s: State, ca: NonEmptyVector[A]): Either[E, Step[State, A, B, R2]] =
      stepPure(s, ca) match {
        case value: Step[State, A, B, R2] => Right(value)
        case Step.Fail(error) => Left(error)
      }

    override def end(s: State, result: R): Either[E, (Vector[B], Either[E, R2])] = {
      val (lastB, r2) = endPure(s, result)
      Right((lastB, r2))
    }

  }

  trait PureSingle[E, A, -R, +B, +R2] extends Single[Either, E, A, R, B, R2] with Pure[E, A, R, B, R2] {

    def stepSinglePure(s: State, a: A): StepPure[State, E, A, B, R2]

    final override def stepPure(s: State, ca: NonEmptyVector[A]): StepPure[State, E, A, B, R2] =
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

    final override def stepSingle(s: State, a: A): Either[E, Step[State, A, B, R2]] =
      stepSinglePure(s, a) match {
        case step: Step[State, A, B, R2] => Right(step)
        case Step.Fail(error) => Left(error)
      }

  }

  def identity[F[+_, +_], E, A, R](implicit applicative: Applicative[F[E, ?]]): StreamTransformation[F, E, A, R, A, R] = new StreamTransformation[F, E, A, R, A, R] {
    override type State = Unit

    type FE[+X] = F[E, X]

    override def initial: FE[Unit] = ().pure[FE]

    override def step(s: Unit, ca: NonEmptyVector[A]): FE[Step[Unit, A, A, R]] =
      Step.Produce((), ca.head, ca.tail).pure[FE]

    override def end(s: Unit, result: R): FE[(Vector[A], FE[R])] =
      (Vector.empty, result.pure[FE]).pure[FE]

  }

  def flattenVector[F[+_, +_], E, A, R](implicit monad: Monad[F[E, ?]]): StreamTransformation[F, E, Vector[A], R, A, R] = new StreamTransformation.Single[F, E, Vector[A], R, A, R] {
    override type State = Unit

    override def initial: F[E, State] = ().pure[F[E, ?]]

    override def stepSingle(s: State, a: Vector[A]): F[E, Step[State, Vector[A], A, R]] =
      a match {
        case head +: tail => Step.Produce((), head, Vector(tail)).pure[F[E, ?]]
        case Vector() => Step.Continue(()).pure[F[E, ?]]
      }


    override def end(s: State, result: R): F[E, (Vector[A], F[E, R])] =
      (Vector.empty, result.pure[F[E, ?]]).pure[F[E, ?]]
  }


  def toVector[F[+_, +_], E, A](implicit monad: Monad[F[E, ?]]): StreamTransformation[F, E, A, Unit, Nothing, Vector[A]] = new StreamTransformation[F, E, A, Unit, Nothing, Vector[A]] {
    override type State = Vector[A]

    override def initial: F[E, State] = Vector().pure[F[E, ?]]

    override def step(s: State, ca: NonEmptyVector[A]): F[E, Step[State, A, Nothing, Vector[A]]] =
      Step.Continue(s ++ ca.toVector).pure[F[E, ?]]

    override def end(s: Vector[A], result: Unit): F[E, (Vector[Nothing], F[E, Vector[A]])] =
      (Vector.empty, s.pure[F[E, ?]]).pure[F[E, ?]]
  }

}
