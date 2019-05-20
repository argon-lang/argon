package dev.argon.util

import cats._
import cats.data._
import cats.implicits._
import scalaz.zio.{stream => zstream, _}
import scalaz.zio.stream.ZStream.Fold
import AnyExtensions._
import cats.evidence.Is
import dev.argon.util.stream.ArStream.EffectConverter

package object stream {

  implicit final class StreamTransformationExtensions[F[+_, +_], E, A, R, B, R2](private val sink: StreamTransformation[F, E, A, R, B, R2]) {

    def into[E2 >: E, B2 >: B, C, R3](other: StreamTransformation[F, E2, B2, R2, C, R3])(implicit monadInstance: Monad[F[E2, ?]]): StreamTransformation[F, E2, A, R, C, R3] =
      new StreamTransformation[F, E2, A, R, C, R3] {

        sealed trait IntoState
        final case class BothRunning(s1: sink.State, chunkB: Vector[B2], s2: other.State) extends IntoState
        final case class FirstFinished(r2: R2, chunkB: Vector[B2], s2: other.State) extends IntoState
        final case class BothFinished(chunkC: Vector[C], fr3: F[E2, R3]) extends IntoState

        override type State = IntoState

        def feToFe2[X](fe: F[E, X]): F[E2, X] = fe

        override def initial: F[E2, State] = for {
          s1 <- feToFe2(sink.initial)
          s2 <- other.initial
        } yield BothRunning(s1, Vector.empty, s2)


        override def step(s: State, ca: NonEmptyVector[A]): F[E2, Step[State, A, C, R3]] =
          s match {
            case BothRunning(s1, chunkB, s2) =>
              feToFe2(sink.step(s1, ca)).flatMap {
                case Step.Produce(s1, value, chunkA) =>
                  other.step(s2, NonEmptyVector.fromVector(chunkB).map { _ :+ value }.getOrElse(NonEmptyVector.of(value))).flatMap {
                    case Step.Produce(s2, value, chunkB) => Step.Produce(BothRunning(s1, chunkB, s2), value, chunkA).pure[F[E2, ?]]
                    case Step.Continue(s2) =>
                      val newState = BothRunning(s1, Vector.empty, s2)
                      NonEmptyVector.fromVector(chunkA) match {
                        case Some(chunkA) => step(newState, chunkA)
                        case None => Step.Continue(newState).pure[F[E2, ?]]
                      }

                    case stop @ Step.Stop(_) => stop.pure[F[E2, ?]]
                  }

                case Step.Continue(s1) => step(BothRunning(s1, chunkB, s2), ca)
                case Step.Stop(r2) => step(FirstFinished(r2, chunkB, s2), ca)
              }

            case FirstFinished(r2, chunkB, s2) =>
              NonEmptyVector.fromVector(chunkB) match {
                case Some(chunkB) => other.step(s2, chunkB).flatMap {
                  case Step.Produce(s2, value, chunkB) => Step.Produce(FirstFinished(r2, chunkB, s2), value, ca.toVector).pure[F[E2, ?]]
                  case Step.Continue(s2) => other.end(s2, r2).flatMap { case (chunkC, fr3) => step(BothFinished(chunkC, fr3), ca) }
                  case done @ Step.Stop(_) => done.pure[F[E2, ?]]
                }

                case None => other.end(s2, r2).flatMap {
                  case (chunkC, fr3) => step(BothFinished(chunkC, fr3), ca)
                }
              }

            case BothFinished(head +: tail, fr3) => Step.Produce(BothFinished(tail, fr3), head, ca.toVector).pure[F[E2, ?]]
            case BothFinished(Vector(), fr3) => fr3.map { r3 => Step.Stop(r3) }
          }

        override def end(s: State, result: R): F[E2, (Vector[C], F[E2, R3])] =
          s match {
            case BothRunning(s1, chunkB, s2) =>
              feToFe2(sink.end(s1, result)).flatMap {
                case (lastB, fr2) =>
                  def feed(s2: other.State, lastB: Vector[B2], acc: Vector[C]): F[E2, (Vector[C], F[E2, R3])] =
                    NonEmptyVector.fromVector(lastB) match {
                      case Some(elems) =>
                        other.step(s2, elems).flatMap {
                          case Step.Produce(s2, value, chunk) => feed(s2, chunk, acc :+ value)
                          case Step.Continue(state) => feed(state, Vector.empty, acc)
                          case Step.Stop(r3) => (acc, r3.pure[F[E2, ?]]).pure[F[E2, ?]]
                        }

                      case None => feToFe2(fr2).flatMap(other.end(s2, _))
                    }

                  feed(s2, chunkB ++ lastB, Vector.empty)
              }

            case FirstFinished(r2, lastB, s2) =>
              def feed(s2: other.State, lastB: Vector[B2], acc: Vector[C]): F[E2, (Vector[C], F[E2, R3])] =
                NonEmptyVector.fromVector(lastB) match {
                  case Some(elems) =>
                    other.step(s2, elems).flatMap {
                      case Step.Produce(s2, value, chunk) => feed(s2, chunk, acc :+ value)
                      case Step.Continue(state) => feed(state, Vector.empty, acc)
                      case Step.Stop(r3) => (acc, r3.pure[F[E2, ?]]).pure[F[E2, ?]]
                    }

                  case None => other.end(s2, r2)
                }

              feed(s2, lastB, Vector.empty)

            case BothFinished(chunkC, fr3) => (chunkC, fr3).pure[F[E2, ?]]
          }

      }


    def buffer(count: Int)(implicit monadInstance: Monad[F[E, ?]]): StreamTransformation[F, E, A, R, B, R2] = new StreamTransformation[F, E, A, R, B, R2] {

      override type State = (Vector[A], sink.State)

      override def initial: F[E, State] = for {
        s <- sink.initial
      } yield (Vector.empty, s)


      override def step(s: State, ca: NonEmptyVector[A]): F[E, Step[State, A, B, R2]] =
        if(s._1.size + ca.length >= count)
          sink.step(s._2, NonEmptyVector.fromVector(s._1).map { _ ++: ca }.getOrElse(ca)).map {
            case Step.Produce(state, value, chunk) => Step.Produce((chunk, state), value, Vector.empty)
            case Step.Continue(state) => Step.Continue((Vector.empty, state))
            case stop @ Step.Stop(_) => stop
          }
        else
          Step.Continue((s._1 ++ ca.toVector, s._2)).pure[F[E, ?]]

      override def end(s: State, result: R): F[E, (Vector[B], F[E, R2])] =
        feedEnd(s._1, s._2, result, Vector.empty)


      private def feedEnd(chunk: Vector[A], s: sink.State, result: R, acc: Vector[B]): F[E, (Vector[B], F[E, R2])] =
        NonEmptyVector.fromVector(chunk) match {
          case Some(ca) => sink.step(s, ca).flatMap {
            case Step.Produce(state, value, chunk) => feedEnd(chunk, state, result, acc :+ value)
            case Step.Continue(state) => feedEnd(Vector.empty, state, result, acc)
            case Step.Stop(result) => (acc, result.pure[F[E, ?]]).pure[F[E, ?]]
          }

          case None => sink.end(s, result)
        }

    }

    def collect[C](f: PartialFunction[B, C])(implicit monadInstance: Monad[F[E, ?]]): StreamTransformation[F, E, A, R, C, R2] = new StreamTransformation[F, E, A, R, C, R2] {

      override type State = sink.State

      override def initial: F[E, sink.State] = sink.initial

      override def step(s: sink.State, ca: NonEmptyVector[A]): F[E, Step[sink.State, A, C, R2]] =
        sink.step(s, ca).flatMap {
          case Step.Produce(state, value, chunk) if f.isDefinedAt(value) => Step.Produce(state, f(value), chunk).pure[F[E, ?]]
          case Step.Produce(state, _, chunk) =>
            NonEmptyVector.fromVector(chunk) match {
              case Some(ca) => step(s, ca)
              case None => Step.Continue(state).pure[F[E, ?]]
            }

          case cont @ Step.Continue(_) => cont.pure[F[E, ?]]
          case stop @ Step.Stop(_) => stop.pure[F[E, ?]]
        }

      override def end(s: sink.State, result: R): F[E, (Vector[C], F[E, R2])] =
        sink.end(s, result).map { case (lastB, r2) => (lastB.collect(f), r2) }

    }

    def map[C](f: B => C)(implicit functor: Functor[F[E, ?]]): StreamTransformation[F, E, A, R, C, R2] = new StreamTransformation[F, E, A, R, C, R2] {

      override type State = sink.State

      override def initial: F[E, sink.State] = sink.initial

      override def step(s: sink.State, ca: NonEmptyVector[A]): F[E, Step[sink.State, A, C, R2]] =
        sink.step(s, ca).map {
          case Step.Produce(state, value, chunk) => Step.Produce(state, f(value), chunk)
          case cont @ Step.Continue(_) => cont
          case stop @ Step.Stop(_) => stop
        }

      override def end(s: sink.State, result: R): F[E, (Vector[C], F[E, R2])] =
        sink.end(s, result).map { case (lastB, r2) => (lastB.map(f), r2) }

    }

    def mapResult[R3](f: R2 => R3)(implicit functor: Functor[F[E, ?]]): StreamTransformation[F, E, A, R, B, R3] = new StreamTransformation[F, E, A, R, B, R3] {

      override type State = sink.State

      override def initial: F[E, sink.State] = sink.initial

      override def step(s: sink.State, ca: NonEmptyVector[A]): F[E, Step[sink.State, A, B, R3]] =
        sink.step(s, ca).map {
          case produce @ Step.Produce(_, _, _) => produce
          case cont @ Step.Continue(_) => cont
          case Step.Stop(result) => Step.Stop(f(result))
        }

      override def end(s: sink.State, result: R): F[E, (Vector[B], F[E, R3])] =
        sink.end(s, result).map { case (lastB, r2) => (lastB, r2.map(f)) }

    }

    def translate[G[+_, +_], E2](f: F[E, ?] ~> G[E2, ?])(implicit functor: Functor[G[E2, ?]]): StreamTransformation[G, E2, A, R, B, R2] = new StreamTransformation[G, E2, A, R, B, R2] {
      override type State = sink.State

      override def initial: G[E2, sink.State] = f(sink.initial)

      override def step(s: sink.State, ca: NonEmptyVector[A]): G[E2, Step[sink.State, A, B, R2]] =
        f(sink.step(s, ca))

      override def end(s: sink.State, result: R): G[E2, (Vector[B], G[E2, R2])] =
        f(sink.end(s, result)).map { case (restB, fr2) => (restB, f(fr2)) }
    }

  }

  implicit final class TransformEitherExtensions[E, A, R, B, R2](private val sink: StreamTransformation[Either, E, A, R, B, R2]) extends AnyVal {

    def covary[F[+_, +_]](implicit applicativeError: ApplicativeError[F[E, ?], E]): StreamTransformation[F, E, A, R, B, R2] =
      new StreamTransformation[F, E, A, R, B, R2] {
        override type State = sink.State

        override def initial: F[E, sink.State] = applicativeError.fromEither(sink.initial)

        override def step(s: sink.State, ca: NonEmptyVector[A]): F[E, Step[sink.State, A, B, R2]] =
          applicativeError.fromEither(sink.step(s, ca))

        override def end(s: sink.State, result: R): F[E, (Vector[B], F[E, R2])] =
          applicativeError.fromEither(sink.end(s, result))
            .map { case (lastB, r2) => (lastB, applicativeError.fromEither(r2)) }
      }

  }

  implicit final class ZStreamExtensions[R, E, A](private val stream: zstream.ZStream[R, E, A]) extends AnyVal {

    private def eitherToIO: ArStream.EffectConverter[Either, IO] = new ArStream.EffectConverter[Either, IO] {
      override def apply[E1, X](fea: Either[E1, X]): IO[E1, X] = IO.fromEither(fea)
    }

    def transformPure[E1 >: E, B](trans: StreamTransformation[Either, E1, A, Unit, B, Unit]): zstream.ZStream[R, E1, B] =
      transform[Either, E1, B](eitherToIO)(trans)

    def transformIO[E1 >: E, B](trans: StreamTransformation[IO, E1, A, Unit, B, Unit]): zstream.ZStream[R, E1, B] =
      transform[IO, E1, B](ArStream.EffectConverter.id)(trans)


    def transform[F[+_, +_], E1 >: E, B](fToIO: ArStream.EffectConverter[F, ZIO[R, ?, ?]])(trans: StreamTransformation[F, E1, A, Unit, B, Unit]): zstream.ZStream[R, E1, B] = new zstream.ZStream[R, E1, B] {
      override def fold[R1 <: R, E2 >: E1, B1 >: B, S]: Fold[R1, E2, B1, S] =
        IO.succeedLazy { (s2, cont, f) =>

          def feed(s1: trans.State, s2: S, chunk: NonEmptyVector[A]): ZIO[R1, E2, (Option[trans.State], S)] =
            fToIO(trans.step(s1, chunk)).flatMap {
              case Step.Produce(s1, value, chunk) =>
                f(s2, value).flatMap { s2 =>
                  NonEmptyVector.fromVector(chunk) match {
                    case Some(chunk) => feed(s1, s2, chunk)
                    case None => IO.succeed((Some(s1), s2))
                  }
                }

              case Step.Continue(s1) => IO.succeed((Some(s1), s2))
              case Step.Stop(_) => IO.succeed((None, s2))
            }

          stream.fold[R1, E2, A, (Option[trans.State], S)].flatMap { f0 =>
            fToIO(trans.initial).flatMap { initialState =>
              f0((Some(initialState), s2), {
                case (Some(_), s2) => cont(s2)
                case (None, _) => false
              }, {
                case ((Some(s1), s2), a) => feed(s1, s2, NonEmptyVector.of(a))
                case ((None, s2), a) => IO.succeed((None, s2))
              }).flatMap {
                case (Some(s1), s2) =>
                  fToIO(trans.end(s1, ())).flatMap {
                    case (lastB, result) =>

                      def iterLastB(s2: S, lastB: Vector[B]): ZIO[R1, E2, S] =
                        lastB match {
                          case Vector() => fToIO(result).const(s2)
                          case head +: tail => f(s2, head).flatMap { s2 => iterLastB(s2, tail) }
                        }

                      iterLastB(s2, lastB)
                  }

                case (None, s2) => IO.succeed(s2)
              }
            }
          }
        }
    }

  }

  implicit final class ArStreamExtensions[F[+_, +_], E, A](private val stream: ArStream[F, E, A]) extends AnyVal {

    def transformWith[B](trans: StreamTransformation[F, E, A, Unit, B, Unit])(implicit monadInstance: Monad[F[E, ?]]): ArStream[F, E, B] = new ArStream[F, E, B] {

      override def foldLeft[E2 >: E, A2 >: B, R](trans2: StreamTransformation[F, E2, A2, Unit, Nothing, R])(implicit monadInstance: Monad[F[E2, ?]]): F[E2, R] =
        stream.foldLeft(trans.into(trans2))


      override def toZStream[E2 >: E](toIO: ArStream.EffectConverter[F, IO]): zstream.Stream[E2, B] =
        stream.toZStream[E2](toIO).transform[F, E2, B](toIO)(trans)


    }

    def forEach[E2 >: E](f: A => F[E2, Unit])(implicit monadInstance: Monad[F[E2, ?]]): F[E2, Unit] =
      stream.foldLeft(new StreamTransformation.Single[F, E2, A, Unit, Nothing, Unit] {
        override type State = Unit

        override def initial: F[E2, Unit] = ().pure[F[E2, ?]]

        override def stepSingle(s: Unit, a: A): F[E2, Step[Unit, A, Nothing, Unit]] =
          f(a).map { _ => Step.Continue(()) }

        override def end(s: Unit, result: Unit): F[E2, (Vector[Nothing], F[E2, Unit])] =
          (Vector.empty, result.pure[F[E2, ?]]).pure[F[E2, ?]]
      })

    def map[B](f: A => B)(implicit monadInstance: Monad[F[E, ?]]): ArStream[F, E, B] = new ArStream[F, E, B] {

      override def foldLeft[E2 >: E, B2 >: B, R](trans: StreamTransformation[F, E2, B2, Unit, Nothing, R])(implicit monadInstance: Monad[F[E2, ?]]): F[E2, R] =
        stream.foldLeft(new StreamTransformation.Single[F, E2, A, Unit, Nothing, R] {
          override type State = trans.State

          override def initial: F[E2, State] = trans.initial

          override def stepSingle(s: State, a: A): F[E2, Step[State, A, Nothing, R]] =
            trans.step(s, NonEmptyVector.of(f(a))).map {
              case Step.Produce(_, value, _) => value
              case cont @ Step.Continue(_) => cont
              case Step.Stop(r2) => Step.Stop(r2)
            }

          override def end(s: State, result: Unit): F[E2, (Vector[Nothing], F[E2, R])] =
            trans.end(s, result)
        })

      override def toZStream[E2 >: E](toIO: EffectConverter[F, IO]): zstream.Stream[E2, B] =
        stream.toZStream(toIO).map(f)

    }

    def flatMap[B](f: A => ArStream[F, E, B])(implicit monadInstance: Monad[F[E, ?]]): ArStream[F, E, B] = new ArStream[F, E, B] {

      override def foldLeft[E2 >: E, B2 >: B, R](trans: StreamTransformation[F, E2, B2, Unit, Nothing, R])(implicit monadInstance: Monad[F[E2, ?]]): F[E2, R] =
        stream.foldLeft(new StreamTransformation.Single[F, E2, A, Unit, Nothing, R] {
          override type State = trans.State

          override def initial: F[E2, State] = trans.initial

          override def stepSingle(s: State, a: A): F[E2, Step[State, A, Nothing, R]] =
            f(a).foldLeft(new StreamTransformation[F, E2, B2, Unit, Nothing, Either[R, trans.State]] {
              override type State = trans.State

              override def initial: F[E2, State] = s.pure[F[E2, ?]]

              override def step(s: State, ca: NonEmptyVector[B2]): F[E2, Step[State, B2, Nothing, Either[R, trans.State]]] =
                trans.step(s, ca).map {
                  case Step.Produce(_, value, _) => value
                  case cont @ Step.Continue(_) => cont
                  case Step.Stop(r2) => Step.Stop(Left(r2))
                }

              override def end(s: trans.State, result: Unit): F[E2, (Vector[Nothing], F[E2, Either[R, trans.State]])] =
                (Vector.empty, Right(s).pure[F[E2, ?]]).pure[F[E2, ?]]
            }).map {
              case Left(r2) => Step.Stop(r2)
              case Right(s) => Step.Continue(s)
            }

          override def end(s: State, result: Unit): F[E2, (Vector[Nothing], F[E2, R])] =
            trans.end(s, result)
        })

      override def toZStream[E2 >: E](toIO: EffectConverter[F, IO]): zstream.Stream[E2, B] =
        stream.toZStream(toIO).flatMap { a => f(a).toZStream(toIO) }

    }

  }

  implicit final class ArStreamVectorExtensions[F[+_, +_], E, A](private val stream: ArStream[F, E, Vector[A]]) extends AnyVal {

    def flattenVector(implicit monadInstance: Monad[F[E, ?]]): ArStream[F, E, A] = new ArStream[F, E, A] {
      override def foldLeft[E2 >: E, A2 >: A, R](trans: StreamTransformation[F, E2, A2, Unit, Nothing, R])(implicit monadInstance: Monad[F[E2, ?]]): F[E2, R] =
        stream.foldLeft(StreamTransformation.flattenVector[F, E2, A2, Unit].into(trans))

      override def toZStream[E2 >: E](toIO: EffectConverter[F, IO]): zstream.Stream[E2, A] =
        stream.toZStream(toIO).flatMap(zstream.ZStream.fromIterable)
    }

  }

  implicit final class EffectConverterExtensions[F[+_, +_], G[+_, +_]](private val effectConverter: ArStream.EffectConverter[F, G]) extends AnyVal {

    def withError[E]: F[E, ?] ~> G[E, ?] = new (F[E, ?] ~> G[E, ?]) {
      override def apply[A](fa: F[E, A]): G[E, A] = effectConverter(fa)
    }

  }

}
