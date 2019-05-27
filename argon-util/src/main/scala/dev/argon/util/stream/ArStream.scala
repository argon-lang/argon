package dev.argon.util.stream

import cats._
import cats.data._
import cats.implicits._
import scalaz.zio.{stream => zstream, _}
import scalaz.zio.interop.catz._
import scalaz.zio.stream.ZStream.Fold

trait ArStream[F[-_, +_, +_], -R, +E, +A] {

  def foldLeft[R2 <: R, E2 >: E, A2 >: A, X](trans: StreamTransformation[F, R2, E2, A2, Unit, Nothing, X])(implicit monadInstance: Monad[F[R2, E2, ?]]): F[R2, E2, X]

  def toZStream(toIO: ArStream.EffectConverter[F, ZIO]): zstream.ZStream[R, E, A]

}

trait ArStreamTranslatable[F[-_, +_, +_], -R, +E, +A] extends ArStream[F, R, E, A] {

  def translate[G[-_, +_, +_]](f: ArStream.EffectConverter[F, G]): ArStream[G, R, E, A]


  override def toZStream(toIO: ArStream.EffectConverter[F, ZIO]): zstream.ZStream[R, E, A] =
    new zstream.ZStream[R, E, A] {
      override def fold[R1 <: R, E2 >: E, A1 >: A, S]: Fold[R1, E2, A1, S] =
        ZIO.succeedLazy { (s, cont, f) =>

          val toZIO = new ArStream.EffectConverter[F, ZIO] {
            override def apply[R2, E3, A2](fea: F[R2, E3, A2]): ZIO[R2, E3, A2] = toIO(fea)
          }

          translate[ZIO](toZIO).foldLeft(new StreamTransformation.Single[ZIO, R1, E2, A1, Unit, Nothing, S] {
            override type State = S
            override def initial: Resource[ZIO, R1, E2, S] = Resource.pure(s)
            override def stepSingle(s: S, a: A1): ZIO[R1, E2, Step[S, A1, Nothing, S]] =
              if(cont(s))
                f(s, a).map { Step.Continue(_) }
              else
                IO.succeed(Step.Stop(s))
            override def end(s: S, result: Unit): ZIO[R1, E2, (Vector[Nothing], ZIO[R1, E2, S])] =
              IO.succeed((Vector.empty, IO.succeed(s)))
          })
        }
    }
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
      stream.fold[R2, E2, A2, Either[X, trans.State]].flatMap { f0 =>
        trans.initial.use { initial =>
          f0(Right(initial), s => s.isRight, {
            case (result @ Left(_), _) => IO.succeed(result)
            case (Right(s), a) => trans.step(s, NonEmptyVector.of(a)).map {
              case Step.Produce(_, value, _) => value
              case Step.Continue(s) => Right(s)
              case Step.Stop(result) => Left(result)
            }
          })
            .flatMap {
              case Left(result) => IO.succeed(result)
              case Right(s) => trans.end(s, ()).flatMap { case (_, res) => res }
            }
        }
      }

    override def toZStream(toIO: EffectConverter[ZIO, ZIO]): zstream.ZStream[R, E, A] = stream
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

    override def toZStream(toIO: EffectConverter[F, ZIO]): zstream.ZStream[R, E, A] = zstream.Stream.fromIterable(coll)
  }

}
