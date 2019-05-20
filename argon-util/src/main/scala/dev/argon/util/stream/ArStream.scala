package dev.argon.util.stream

import cats._
import cats.data._
import cats.implicits._
import scalaz.zio.{stream => zstream, _}
import scalaz.zio.interop.catz._
import scalaz.zio.stream.ZStream.Fold

trait ArStream[F[+_, +_], +E, +A] {

  def foldLeft[E2 >: E, A2 >: A, R](trans: StreamTransformation[F, E2, A2, Unit, Nothing, R])(implicit monadInstance: Monad[F[E2, ?]]): F[E2, R]

  def toZStream[E2 >: E](toIO: ArStream.EffectConverter[F, IO]): zstream.Stream[E2, A]

}

trait ArStreamTranslatable[F[+_, +_], +E, +A] extends ArStream[F, E, A] {

  def translate[G[+_, +_], E2 >: E](f: ArStream.EffectConverter[F, G]): ArStream[G, E2, A]


  override def toZStream[E2 >: E](toIO: ArStream.EffectConverter[F, IO]): zstream.Stream[E2, A] =
    new zstream.Stream[E2, A] {
      override def fold[R1 <: Any, E3 >: E2, A1 >: A, S]: Fold[R1, E3, A1, S] =
        ZIO.succeedLazy { (s, cont, f) =>

          val toZIO = new ArStream.EffectConverter[F, ZIO[R1, +?, +?]] {
            override def apply[E4, A2](fea: F[E4, A2]): ZIO[R1, E4, A2] = toIO(fea)
          }

          translate[ZIO[R1, +?, +?], E3](toZIO).foldLeft(new StreamTransformation.Single[ZIO[R1, +?, +?], E3, A1, Unit, Nothing, S] {
            override type State = S
            override def initial: ZIO[R1, E3, S] = IO.succeed(s)
            override def stepSingle(s: S, a: A1): ZIO[R1, E3, Step[S, A1, Nothing, S]] =
              if(cont(s))
                f(s, a).map { Step.Continue(_) }
              else
                IO.succeed(Step.Stop(s))
            override def end(s: S, result: Unit): ZIO[R1, E3, (Vector[Nothing], ZIO[R1, E3, S])] =
              IO.succeed((Vector.empty, IO.succeed(s)))
          })
        }
    }
}

object ArStream {

  trait EffectConverter[-F[_, _], +G[_, _]] {
    def apply[E, A](fea: F[E, A]): G[E, A]

    final def andThen[H[_, _]](other: EffectConverter[G, H]): EffectConverter[F, H] = new EffectConverter[F, H] {
      override def apply[E, A](fea: F[E, A]): H[E, A] = other(EffectConverter.this(fea))
    }

  }

  object EffectConverter {

    def id[F[_, _]]: EffectConverter[F, F] = new EffectConverter[F, F] {
      override def apply[E, A](fea: F[E, A]): F[E, A] = fea
    }

  }

  def fromZStream[E, A](stream: zstream.Stream[E, A]): ArStream[IO, E, A] = new ArStream[IO, E, A] {

    override def foldLeft[E2 >: E, A2 >: A, R](trans: StreamTransformation[IO, E2, A2, Unit, Nothing, R])(implicit monadInstance: Monad[IO[E2, ?]]): IO[E2, R] =
      stream.fold[Any, E2, A2, Either[R, trans.State]].flatMap { f0 =>
        trans.initial
          .flatMap { initial =>
            f0(Right(initial), s => s.isRight, {
              case (result @ Left(_), _) => IO.succeed(result)
              case (Right(s), a) => trans.step(s, NonEmptyVector.of(a)).map {
                case Step.Produce(_, value, _) => value
                case Step.Continue(s) => Right(s)
                case Step.Stop(result) => Left(result)
              }
            })
          }
          .flatMap {
            case Left(result) => IO.succeed(result)
            case Right(s) => trans.end(s, ()).flatMap { case (_, res) => res }
          }
      }

    override def toZStream[E2 >: E](toIO: EffectConverter[IO, IO]): zstream.Stream[E, A] = stream
  }

  def fromVector[F[+_, +_], E, A](coll: Vector[A]): ArStream[F, E, A] = new ArStream[F, E, A] {

    override def foldLeft[E2 >: E, A2 >: A, R](trans: StreamTransformation[F, E2, A2, Unit, Nothing, R])(implicit monadInstance: Monad[F[E2, ?]]): F[E2, R] = {

      def feed(s: trans.State, coll: Vector[A]): F[E2, R] =
        coll match {
          case head +: tail => trans.step(s, NonEmptyVector.of(head)).flatMap {
            case Step.Produce(_, value, _) => value
            case Step.Continue(s) => feed(s, tail)
            case Step.Stop(result) => result.pure[F[E2, ?]]
          }
          case Vector() => trans.end(s, ()).flatMap { case (_, result) => result }
        }

      trans.initial.flatMap { s =>
        feed(s, coll)
      }
    }

    override def toZStream[E2 >: E](toIO: EffectConverter[F, IO]): zstream.Stream[E, A] = zstream.Stream.fromIterable(coll)
  }

}
