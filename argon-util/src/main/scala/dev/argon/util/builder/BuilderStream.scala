package dev.argon.util.builder

import cats._
import cats.implicits._
import cats.data.NonEmptyVector
import dev.argon.util.stream._
import dev.argon.util.AnyExtensions._
import scalaz.zio.ZIO
import scalaz.zio.stream.ZStream

sealed trait BuilderStream[F[_], S, A] {

  def map[B](f: A => B)(implicit functor: Functor[F]): BuilderStream[F, S, B]
  def flatMap[B](f: A => BuilderStream[F, S, B])(implicit functor: Functor[F]): BuilderStream[F, S, B]

}

object BuilderStream {

  final case class Emit[F[_], S, A](value: S, next: BuilderStream[F, S, A]) extends BuilderStream[F, S, A] {
    override def map[B](f: A => B)(implicit functor: Functor[F]): BuilderStream[F, S, B] = Emit(value, next.map(f))

    override def flatMap[B](f: A => BuilderStream[F, S, B])(implicit functor: Functor[F]): BuilderStream[F, S, B] =
      Emit(value, next.flatMap(f))
  }
  final case class Effect[F[_], S, A](fa: F[BuilderStream[F, S, A]]) extends BuilderStream[F, S, A] {
    override def map[B](f: A => B)(implicit functor: Functor[F]): BuilderStream[F, S, B] =
      Effect(fa.map { builder => builder.map(f) })

    override def flatMap[B](f: A => BuilderStream[F, S, B])(implicit functor: Functor[F]): BuilderStream[F, S, B] =
      Effect(fa.map { builder => builder.flatMap(f) })
  }
  final case class Result[F[_], S, A](value: A) extends BuilderStream[F, S, A] {
    override def map[B](f: A => B)(implicit functor: Functor[F]): BuilderStream[F, S, B] = Result(f(value))
    override def flatMap[B](f: A => BuilderStream[F, S, B])(implicit functor: Functor[F]): BuilderStream[F, S, B] =
      f(value)
  }


  def toStream[F[-_, +_, +_], R, E, S](builder: BuilderStream[F[R, E, ?], S, Unit])(implicit functor: Functor[F[R, E, ?]]): ArStream[F, R, E, S] =
    new ArStream[F, R, E, S] {
      override def foldLeft[R2 <: R, E2 >: E, S2 >: S, X](trans: StreamTransformation[F, R2, E2, S2, Unit, Nothing, X])(implicit monadInstance: Monad[F[R2, E2, ?]]): F[R2, E2, X] = {

        def feed(state: trans.State, builder: BuilderStream[F[R, E, ?], S, Unit]): F[R2, E2, X] =
          builder match {
            case Emit(value, next) =>
              trans.step(state, NonEmptyVector.of(value)).flatMap {
                case Step.Produce(_, value, _) => value
                case Step.Continue(state) => feed(state, next)
                case Step.Stop(result) => monadInstance.pure(result)
              }

            case Effect(fa) => fa.upcast[F[R2, E2, BuilderStream[F[R, E, ?], S, Unit]]].flatMap { builder => feed(state, builder) }
            case Result(value) => trans.end(state, value).flatMap { case (_, fx) => fx }
          }

        trans.initial.use { state =>
          feed(state, builder)
        }
      }

    }

  implicit def builderStreamBuilderInstance[F[_], S](implicit monad: Monad[F]): Builder[BuilderStream[F, S, ?], S] =
    new Builder[BuilderStream[F, S, ?], S] with StackSafeMonad[BuilderStream[F, S, ?]] {
      override def append(value: S): BuilderStream[F, S, Unit] = Emit(value, Result(()))

      override def flatMap[A, B](fa: BuilderStream[F, S, A])(f: A => BuilderStream[F, S, B]): BuilderStream[F, S, B] =
        fa.flatMap(f)

      override def pure[A](x: A): BuilderStream[F, S, A] = Result(x)
    }

}
