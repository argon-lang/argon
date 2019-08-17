package dev.argon.stream

import cats._
import cats.data._
import cats.implicits._
import dev.argon.stream.builder.Iter
import zio.{stream => zstream, _}

trait ArStream[F[-_, +_, +_], -R, +E, +A] {

  def foldLeft[R2 <: R, E2 >: E, A2 >: A, X](trans: StreamTransformation[F, R2, E2, A2, Unit, Nothing, X])(implicit monadInstance: Monad[F[R2, E2, ?]]): F[R2, E2, X]

}

object ArStream {

  trait EffectConverter[-F[_, _, _], +G[_, _, _]] {
    def apply[R, E, A](fea: F[R, E, A]): G[R, E, A]

    final def andThen[H[_, _, _]](other: EffectConverter[G, H]): EffectConverter[F, H] = new EffectConverter[F, H] {
      override def apply[R, E, A](fea: F[R, E, A]): H[R, E, A] = other(EffectConverter.this(fea))
    }

  }

  object EffectConverter {

    def id[F[_, _, _]]: EffectConverter[F, F] = new EffectConverter[F, F] {
      override def apply[R, E, A](fea: F[R, E, A]): F[R, E, A] = fea
    }

  }

  def fromZStream[R, E, A](stream: zstream.ZStream[R, E, A]): ArStream[ZIO, R, E, A] = new ArStream[ZIO, R, E, A] {

    override def foldLeft[R2 <: R, E2 >: E, A2 >: A, X](trans: StreamTransformation[ZIO, R2, E2, A2, Unit, Nothing, X])(implicit monadInstance: Monad[ZIO[R2, E2, ?]]): ZIO[R2, E2, X] =
      stream.fold[R2, E2, A2, Either[X, trans.State]].use { f0 =>
        trans.initial.use { initial =>
          f0(Right(initial), s => s.isRight, {
            case (result @ Left(_), _) => IO.succeed(result)
            case (Right(s), a) => trans.step(s, NonEmptyVector.of(a)).map {
              case Step.Produce(_, value, _) => value
              case Step.Continue(s) => Right(s)
              case Step.Stop(result) => Left(result)
            }
          })
            .use {
              case Left(result) => IO.succeed(result)
              case Right(s) => trans.end(s, ()).flatMap { case (_, res) => res }
            }
        }
      }
  }

  def fromVector[F[-_, +_, +_], R, E, A](coll: Vector[A]): ArStream[F, R, E, A] = new ArStream[F, R, E, A] {



    override def foldLeft[R2 <: R, E2 >: E, A2 >: A, X](trans: StreamTransformation[F, R2, E2, A2, Unit, Nothing, X])(implicit monadInstance: Monad[F[R2, E2, ?]]): F[R2, E2, X] = {

      def feed(s: trans.State, coll: Vector[A]): F[R2, E2, X] =
        coll match {
          case head +: tail => trans.step(s, NonEmptyVector.of(head)).flatMap {
            case Step.Produce(_, value, _) => value
            case Step.Continue(s) => feed(s, tail)
            case Step.Stop(result) => result.pure[F[R2, E2, ?]]
          }
          case Vector() => trans.end(s, ()).flatMap { case (_, result) => result }
        }

      trans.initial.use { s =>
        feed(s, coll)
      }
    }

  }

  def fromWrapped[F[-_, +_, +_], R, E, A](value: F[R, E, ArStream[F, R, E, A]]): ArStream[F, R, E, A] =
    new ArStream[F, R, E, A] {
      override def foldLeft[R2 <: R, E2 >: E, A2 >: A, X](trans: StreamTransformation[F, R2, E2, A2, Unit, Nothing, X])(implicit monadInstance: Monad[F[R2, E2, ?]]): F[R2, E2, X] =
        (value : F[R2, E2, ArStream[F, R, E, A]]).flatMap { _.foldLeft(trans) }
    }

}
