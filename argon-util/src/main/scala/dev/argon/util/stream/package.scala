package dev.argon.util

import cats._
import cats.data._
import cats.implicits._
import scalaz.zio.{stream => zstream, _}
import scalaz.zio.stream.ZStream.Fold
import AnyExtensions._
import cats.evidence.Is
import dev.argon.util.stream.ArStream.EffectConverter
import scalaz.zio.stream.ZSink

package object stream {

  implicit val zioMapErrorInstance: MapError[ZIO] = new MapError[ZIO] {
    override def mapError[R, E, E2, A](fe: ZIO[R, E, A])(f: E => E2): ZIO[R, E2, A] = fe.mapError(f)
  }


  implicit final class StreamTransformationExtensions[F[-_, +_, +_], R, E, A, X, B, Y](private val sink: StreamTransformation[F, R, E, A, X, B, Y]) {


    def buffer(count: Int)(implicit monadInstance: Monad[F[R, E, ?]]): StreamTransformation[F, R, E, A, X, B, Y] = new StreamTransformation[F, R, E, A, X, B, Y] {

      override type State = (Vector[A], sink.State)

      override def initial: Resource[F, R, E, State] = for {
        s <- sink.initial
      } yield (Vector.empty, s)


      override def step(s: State, ca: NonEmptyVector[A]): F[R, E, Step[State, A, B, Y]] =
        if(s._1.size + ca.length >= count)
          sink.step(s._2, NonEmptyVector.fromVector(s._1).map { _ ++: ca }.getOrElse(ca)).map {
            case Step.Produce(state, value, chunk) => Step.Produce((chunk, state), value, Vector.empty)
            case Step.Continue(state) => Step.Continue((Vector.empty, state))
            case stop @ Step.Stop(_) => stop
          }
        else
          Step.Continue((s._1 ++ ca.toVector, s._2)).pure[F[R, E, ?]]

      override def end(s: State, result: X): F[R, E, (Vector[B], F[R, E, Y])] =
        feedEnd(s._1, s._2, result, Vector.empty)


      private def feedEnd(chunk: Vector[A], s: sink.State, result: X, acc: Vector[B]): F[R, E, (Vector[B], F[R, E, Y])] =
        NonEmptyVector.fromVector(chunk) match {
          case Some(ca) => sink.step(s, ca).flatMap {
            case Step.Produce(state, value, chunk) => feedEnd(chunk, state, result, acc :+ value)
            case Step.Continue(state) => feedEnd(Vector.empty, state, result, acc)
            case Step.Stop(result) => (acc, result.pure[F[R, E, ?]]).pure[F[R, E, ?]]
          }

          case None => sink.end(s, result).map { case (lastB, fr2) => (acc ++ lastB, fr2) }
        }

    }

    def collect[C](f: PartialFunction[B, C])(implicit monadInstance: Monad[F[R, E, ?]]): StreamTransformation[F, R, E, A, X, C, Y] = new StreamTransformation[F, R, E, A, X, C, Y] {

      override type State = sink.State

      override def initial: Resource[F, R, E, sink.State] = sink.initial

      override def step(s: sink.State, ca: NonEmptyVector[A]): F[R, E, Step[sink.State, A, C, Y]] =
        sink.step(s, ca).flatMap {
          case Step.Produce(state, value, chunk) if f.isDefinedAt(value) => Step.Produce(state, f(value), chunk).pure[F[R, E, ?]]
          case Step.Produce(state, _, chunk) =>
            NonEmptyVector.fromVector(chunk) match {
              case Some(ca) => step(s, ca)
              case None => Step.Continue(state).pure[F[R, E, ?]]
            }

          case cont @ Step.Continue(_) => cont.pure[F[R, E, ?]]
          case stop @ Step.Stop(_) => stop.pure[F[R, E, ?]]
        }

      override def end(s: sink.State, result: X): F[R, E, (Vector[C], F[R, E, Y])] =
        sink.end(s, result).map { case (lastB, r2) => (lastB.collect(f), r2) }

    }

    def map[C](f: B => C)(implicit functor: Functor[F[R, E, ?]]): StreamTransformation[F, R, E, A, X, C, Y] = new StreamTransformation[F, R, E, A, X, C, Y] {

      override type State = sink.State

      override def initial: Resource[F, R, E, sink.State] = sink.initial

      override def step(s: sink.State, ca: NonEmptyVector[A]): F[R, E, Step[sink.State, A, C, Y]] =
        sink.step(s, ca).map {
          case Step.Produce(state, value, chunk) => Step.Produce(state, f(value), chunk)
          case cont @ Step.Continue(_) => cont
          case stop @ Step.Stop(_) => stop
        }

      override def end(s: sink.State, result: X): F[R, E, (Vector[C], F[R, E, Y])] =
        sink.end(s, result).map { case (lastB, r2) => (lastB.map(f), r2) }

    }

    def mapResult[Z](f: Y => Z)(implicit functor: Functor[F[R, E, ?]]): StreamTransformation[F, R, E, A, X, B, Z] = new StreamTransformation[F, R, E, A, X, B, Z] {

      override type State = sink.State

      override def initial: Resource[F, R, E, sink.State] = sink.initial

      override def step(s: sink.State, ca: NonEmptyVector[A]): F[R, E, Step[sink.State, A, B, Z]] =
        sink.step(s, ca).map {
          case produce @ Step.Produce(_, _, _) => produce
          case cont @ Step.Continue(_) => cont
          case Step.Stop(result) => Step.Stop(f(result))
        }

      override def end(s: sink.State, result: X): F[R, E, (Vector[B], F[R, E, Z])] =
        sink.end(s, result).map { case (lastB, r2) => (lastB, r2.map(f)) }

    }

    def mapError[E2](f: E => E2)(implicit mapError: MapError[F], functor: Functor[F[R, E2, ?]]): StreamTransformation[F, R, E2, A, X, B, Y] = new StreamTransformation[F, R, E2, A, X, B, Y] {

      override type State = sink.State

      override def initial: Resource[F, R, E2, sink.State] = sink.initial.mapError(f)

      override def step(s: sink.State, ca: NonEmptyVector[A]): F[R, E2, Step[sink.State, A, B, Y]] =
        mapError.mapError(sink.step(s, ca))(f)

      override def end(s: sink.State, result: X): F[R, E2, (Vector[B], F[R, E2, Y])] =
        mapError.mapError(sink.end(s, result))(f).map { case (lastB, r2) => (lastB, mapError.mapError(r2)(f)) }

    }

  }

  implicit final class ZStreamExtensions[R, E, A](private val stream: zstream.ZStream[R, E, A]) extends AnyVal {

    private def pureToIO: ArStream.EffectConverter[PureEffect, ZIO] = new ArStream.EffectConverter[PureEffect, ZIO] {
      override def apply[R1, E1, X](fea: PureEffect[R1, E1, X]): ZIO[R1, E1, X] = ZIO.accessM(r => ZIO.fromEither(fea.run(r).value))
    }

    def transformPure[R1 <: R, E1 >: E, B](trans: StreamTransformation[PureEffect, R1, E1, A, Unit, B, Unit]): zstream.ZStream[R1, E1, B] =
      transform[PureEffect, R1, E1, B](pureToIO)(trans)

    def transformIO[R1 <: R, E1 >: E, B](trans: StreamTransformation[ZIO, R1, E1, A, Unit, B, Unit]): zstream.ZStream[R1, E1, B] =
      transform[ZIO, R1, E1, B](ArStream.EffectConverter.id)(trans)


    def transform[F[-_, +_, +_], R1 <: R, E1 >: E, B](fToIO: ArStream.EffectConverter[F, ZIO])(trans: StreamTransformation[F, R1, E1, A, Unit, B, Unit]): zstream.ZStream[R1, E1, B] = new zstream.ZStream[R1, E1, B] {
      override def fold[R2 <: R1, E2 >: E1, B1 >: B, S]: Fold[R2, E2, B1, S] =
        IO.succeedLazy { (s2, cont, f) =>

          def feed(s1: trans.State, s2: S, chunk: NonEmptyVector[A]): ZIO[R2, E2, (Option[trans.State], S)] =
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

          stream.fold[R2, E2, A, (Option[trans.State], S)].flatMap { f0 =>
            trans.initial.useIO { initialState =>
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

                      def iterLastB(s2: S, lastB: Vector[B]): ZIO[R2, E2, S] =
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

  implicit final class ArStreamExtensions[F[-_, +_, +_], R, E, A](private val stream: ArStream[F, R, E, A]) extends AnyVal {

    def transformWith[B](trans: StreamTransformation[F, R, E, A, Unit, B, Unit]): ArStream[F, R, E, B] = new ArStream[F, R, E, B] {

      override def foldLeft[R2 <: R, E2 >: E, A2 >: B, X](trans2: StreamTransformation[F, R2, E2, A2, Unit, Nothing, X])(implicit monadInstance: Monad[F[R2, E2, ?]]): F[R2, E2,X] =
        stream.foldLeft(trans.into(trans2))


      override def toZStream(toIO: ArStream.EffectConverter[F, ZIO]): zstream.ZStream[R, E, B] =
        stream.toZStream(toIO).transform[F, R, E, B](toIO)(trans)


    }

    def forEach[R2 <: R, E2 >: E](f: A => F[R2, E2, Unit])(implicit monadInstance: Monad[F[R2, E2, ?]]): F[R2, E2, Unit] =
      stream.foldLeft(new StreamTransformation.Single[F, R2, E2, A, Unit, Nothing, Unit] {
        override type State = Unit

        override def initial: Resource[F, R2, E2, Unit] = Resource.pure(())

        override def stepSingle(s: Unit, a: A): F[R2, E2, Step[Unit, A, Nothing, Unit]] =
          f(a).map { _ => Step.Continue(()) }

        override def end(s: Unit, result: Unit): F[R2, E2, (Vector[Nothing], F[R2, E2, Unit])] =
          (Vector.empty, result.pure[F[R2, E2, ?]]).pure[F[R2, E2, ?]]
      })

    def map[B](f: A => B)(implicit monadInstance: Monad[F[R, E, ?]]): ArStream[F, R, E, B] = new ArStream[F, R, E, B] {

      override def foldLeft[R2 <: R, E2 >: E, B2 >: B, X](trans: StreamTransformation[F, R2, E2, B2, Unit, Nothing, X])(implicit monadInstance: Monad[F[R2, E2, ?]]): F[R2, E2, X] =
        stream.foldLeft(new StreamTransformation.Single[F, R2, E2, A, Unit, Nothing, X] {
          override type State = trans.State

          override def initial: Resource[F, R2, E2, State] = trans.initial

          override def stepSingle(s: State, a: A): F[R2, E2, Step[State, A, Nothing, X]] =
            trans.step(s, NonEmptyVector.of(f(a))).map {
              case Step.Produce(_, value, _) => value
              case cont @ Step.Continue(_) => cont
              case Step.Stop(r2) => Step.Stop(r2)
            }

          override def end(s: State, result: Unit): F[R2, E2, (Vector[Nothing], F[R2, E2, X])] =
            trans.end(s, result)
        })

      override def toZStream(toIO: EffectConverter[F, ZIO]): zstream.ZStream[R, E, B] =
        stream.toZStream(toIO).map(f)

    }

    def flatMap[B](f: A => ArStream[F, R, E, B])(implicit monadInstance: Monad[F[R, E, ?]]): ArStream[F, R, E, B] = new ArStream[F, R, E, B] {

      override def foldLeft[R2 <: R, E2 >: E, B2 >: B, X](trans: StreamTransformation[F, R2, E2, B2, Unit, Nothing, X])(implicit monadInstance: Monad[F[R2, E2, ?]]): F[R2, E2, X] =
        stream.foldLeft(new StreamTransformation.Single[F, R2, E2, A, Unit, Nothing, X] {
          override type State = trans.State

          override def initial: Resource[F, R2, E2, State] = trans.initial

          override def stepSingle(s: State, a: A): F[R2, E2, Step[State, A, Nothing, X]] =
            f(a).foldLeft(new StreamTransformation[F, R2, E2, B2, Unit, Nothing, Either[X, trans.State]] {
              override type State = trans.State

              override def initial: Resource[F, R2, E2, State] = Resource.pure(s)

              override def step(s: State, ca: NonEmptyVector[B2]): F[R2, E2, Step[State, B2, Nothing, Either[X, trans.State]]] =
                trans.step(s, ca).map {
                  case Step.Produce(_, value, _) => value
                  case cont @ Step.Continue(_) => cont
                  case Step.Stop(r2) => Step.Stop(Left(r2))
                }

              override def end(s: trans.State, result: Unit): F[R2, E2, (Vector[Nothing], F[R2, E2, Either[X, trans.State]])] =
                (Vector.empty, Right(s).pure[F[R2, E2, ?]]).pure[F[R2, E2, ?]]
            }).map {
              case Left(r2) => Step.Stop(r2)
              case Right(s) => Step.Continue(s)
            }

          override def end(s: State, result: Unit): F[R2, E2, (Vector[Nothing], F[R2, E2, X])] =
            trans.end(s, result)
        })

      override def toZStream(toIO: EffectConverter[F, ZIO]): zstream.ZStream[R, E, B] =
        stream.toZStream(toIO).flatMap { a => f(a).toZStream(toIO) }

    }

  }

  implicit final class ArStreamVectorExtensions[F[-_, +_, +_], R, E, A](private val stream: ArStream[F, R, E, Vector[A]]) extends AnyVal {

    def flattenVector(implicit monadInstance: Monad[F[R, E, ?]]): ArStream[F, R, E, A] = new ArStream[F, R, E, A] {
      override def foldLeft[R2 <: R, E2 >: E, A2 >: A, X](trans: StreamTransformation[F, R2, E2, A2, Unit, Nothing, X])(implicit monadInstance: Monad[F[R2, E2, ?]]): F[R2, E2, X] =
        stream.foldLeft(StreamTransformation.flattenVector[F, R2, E2, A2, Unit].into(trans))

      override def toZStream(toIO: EffectConverter[F, ZIO]): zstream.ZStream[R, E, A] =
        stream.toZStream(toIO).flatMap(zstream.ZStream.fromIterable)
    }

  }

  implicit final class EffectConverterExtensions[F[-_, +_, +_], G[-_, +_, +_]](private val effectConverter: ArStream.EffectConverter[F, G]) extends AnyVal {

    def toFunctionK[R, E]: F[R, E, ?] ~> G[R, E, ?] = new (F[R, E, ?] ~> G[R, E, ?]) {
      override def apply[A](fa: F[R, E, A]): G[R, E, A] = effectConverter(fa)
    }

  }


}
