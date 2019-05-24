package dev.argon.util.stream

import cats._
import cats.data._
import cats.implicits._
import scalaz.zio._
import scalaz.zio.stream.ZSink

trait StreamTransformation[F[-_, +_, +_], -R, +E, A, -X, +B, +Y] {

  type State

  def initial: F[R, E, State]

  def step(s: State, ca: NonEmptyVector[A]): F[R, E, Step[State, A, B, Y]]

  def end(s: State, result: X): F[R, E, (Vector[B], F[R, E, Y])]


  def into[R2 <: R, E2 >: E, B2 >: B, C, Z](other: StreamTransformation[F, R2, E2, B2, Y, C, Z])(implicit monadInstance: Monad[F[R2, E2, ?]]): StreamTransformation[F, R2, E2, A, X, C, Z] =
    new StreamTransformation[F, R2, E2, A, X, C, Z] {

      sealed trait IntoState
      final case class BothRunning(s1: StreamTransformation.this.State, chunkB: Vector[B2], s2: other.State) extends IntoState
      final case class FirstFinished(r2: Y, chunkB: Vector[B2], s2: other.State) extends IntoState
      final case class BothFinished(chunkC: Vector[C], fr3: F[R2, E2, Z]) extends IntoState

      override type State = IntoState

      def feToFe2[W](fe: F[R, E, W]): F[R2, E2, W] = fe

      override def initial: F[R2, E2, State] = for {
        s1 <- feToFe2(StreamTransformation.this.initial)
        s2 <- other.initial
      } yield BothRunning(s1, Vector.empty, s2)


      override def step(s: State, ca: NonEmptyVector[A]): F[R2, E2, Step[State, A, C, Z]] =
        s match {
          case BothRunning(s1, chunkB, s2) =>
            feToFe2(StreamTransformation.this.step(s1, ca)).flatMap {
              case Step.Produce(s1, value, chunkA) =>
                other.step(s2, NonEmptyVector.fromVector(chunkB).map { _ :+ value }.getOrElse(NonEmptyVector.of(value))).flatMap {
                  case Step.Produce(s2, value, chunkB) => Step.Produce(BothRunning(s1, chunkB, s2), value, chunkA).pure[F[R2, E2, ?]]
                  case Step.Continue(s2) =>
                    val newState = BothRunning(s1, Vector.empty, s2)
                    NonEmptyVector.fromVector(chunkA) match {
                      case Some(chunkA) => step(newState, chunkA)
                      case None => Step.Continue(newState).pure[F[R2, E2, ?]]
                    }

                  case stop @ Step.Stop(_) => stop.pure[F[R2, E2, ?]]
                }

              case Step.Continue(s1) => Step.Continue(BothRunning(s1, chunkB, s2)).pure[F[R2, E2, ?]]
              case Step.Stop(r2) => Step.Continue(FirstFinished(r2, chunkB, s2)).pure[F[R2, E2, ?]]
            }

          case FirstFinished(r2, chunkB, s2) =>
            NonEmptyVector.fromVector(chunkB) match {
              case Some(chunkB) => other.step(s2, chunkB).flatMap {
                case Step.Produce(s2, value, chunkB) => Step.Produce(FirstFinished(r2, chunkB, s2), value, ca.toVector).pure[F[R2, E2, ?]]
                case Step.Continue(s2) => other.end(s2, r2).flatMap { case (chunkC, fr3) => step(BothFinished(chunkC, fr3), ca) }
                case done @ Step.Stop(_) => done.pure[F[R2, E2, ?]]
              }

              case None => other.end(s2, r2).flatMap {
                case (chunkC, fr3) => step(BothFinished(chunkC, fr3), ca)
              }
            }

          case BothFinished(head +: tail, fr3) => Step.Produce(BothFinished(tail, fr3), head, ca.toVector).pure[F[R2, E2, ?]]
          case BothFinished(Vector(), fr3) => fr3.map { r3 => Step.Stop(r3) }
        }

      override def end(s: State, result: X): F[R2, E2, (Vector[C], F[R2, E2, Z])] =
        s match {
          case BothRunning(s1, chunkB, s2) =>
            feToFe2(StreamTransformation.this.end(s1, result)).flatMap {
              case (lastB, fr2) =>
                def feed(s2: other.State, lastB: Vector[B2], acc: Vector[C]): F[R2, E2, (Vector[C], F[R2, E2, Z])] =
                  NonEmptyVector.fromVector(lastB) match {
                    case Some(elems) =>
                      other.step(s2, elems).flatMap {
                        case Step.Produce(s2, value, chunk) => feed(s2, chunk, acc :+ value)
                        case Step.Continue(state) => feed(state, Vector.empty, acc)
                        case Step.Stop(r3) => (acc, r3.pure[F[R2, E2, ?]]).pure[F[R2, E2, ?]]
                      }

                    case None =>
                      feToFe2(fr2).flatMap { r2 =>
                        other.end(s2, r2).map {
                          case (lastC, fr3) => (acc ++ lastC, fr3)
                        }
                      }
                  }

                feed(s2, chunkB ++ lastB, Vector.empty)
            }

          case FirstFinished(r2, lastB, s2) =>
            def feed(s2: other.State, lastB: Vector[B2], acc: Vector[C]): F[R2, E2, (Vector[C], F[R2, E2, Z])] =
              NonEmptyVector.fromVector(lastB) match {
                case Some(elems) =>
                  other.step(s2, elems).flatMap {
                    case Step.Produce(s2, value, chunk) => feed(s2, chunk, acc :+ value)
                    case Step.Continue(state) => feed(state, Vector.empty, acc)
                    case Step.Stop(r3) => (acc, r3.pure[F[R2, E2, ?]]).pure[F[R2, E2, ?]]
                  }

                case None => other.end(s2, r2).map { case (lastC, fr3) => (acc ++ lastC, fr3) }
              }

            feed(s2, lastB, Vector.empty)

          case BothFinished(chunkC, fr3) => (chunkC, fr3).pure[F[R2, E2, ?]]
        }

    }


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
