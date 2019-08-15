package dev.argon.stream.builder

import cats._
import cats.implicits._

sealed trait StepBuilder[F[_], A, X]
final case class Produce[F[_], A, X](value: A, next: F[StepBuilder[F, A, X]]) extends StepBuilder[F, A, X]
final case class Result[F[_], A, X](value: X) extends StepBuilder[F, A, X]

object StepBuilder {

  implicit def stepBuilderBuilderInstance[F[_]: Monad, A]: Builder[Lambda[X => F[StepBuilder[F, A, X]]], A] = new Builder[Lambda[X => F[StepBuilder[F, A, X]]], A] {
    override def append(value: A): F[StepBuilder[F, A, Unit]] =
      Monad[F].pure(Produce(value, pure(())))

    override def pure[B](x: B): F[StepBuilder[F, A, B]] =
      Monad[F].pure(Result(x))

    override def flatMap[B, C](fb: F[StepBuilder[F, A, B]])(f: B => F[StepBuilder[F, A, C]]): F[StepBuilder[F, A, C]] =
      Monad[F].flatMap(fb) {
        case Produce(value, next) => Monad[F].pure(Produce(value, flatMap(next)(f)))
        case Result(value) => f(value)
      }


    override def tailRecM[B, C](b: B)(f: B => F[StepBuilder[F, A, Either[B, C]]]): F[StepBuilder[F, A, C]] =
      f(b).flatMap { sb =>
        Monad[F].tailRecM(sb) {
          case Produce(value, next) => Monad[F].map(next) { sb2 => Left(Produce(value, Monad[F].pure(sb2))) }
          case Result(Left(value)) => Monad[F].pure(Left(Result(Left(value))))
          case Result(Right(value)) => Monad[F].pure(Right(Result(value)))
        }
      }
  }

  implicit def stepBuilderIterInstance[F[_]: Monad]: Iter[F, StepBuilder[F, ?, Unit]] = new Iter[F, StepBuilder[F, ?, Unit]] {
    override def foldLeftM[A, S](data: StepBuilder[F, A, Unit])(state: S)(f: (S, A) => F[S]): F[S] =
      data match {
        case Produce(value, next) =>
          f(state, value).flatMap { newState =>
            next.flatMap(foldLeftM(_)(newState)(f))
          }

        case Result(_) =>
          state.pure[F]
      }
  }

}