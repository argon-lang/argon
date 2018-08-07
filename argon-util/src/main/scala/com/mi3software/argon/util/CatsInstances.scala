package com.mi3software.argon.util

import scalaz._
import shims._

object CatsInstances {
  implicit def scalazEitherTSync[F[_]: cats.effect.Sync, L]: cats.effect.Sync[EitherT[F, L, ?]] =
    new cats.effect.Sync[EitherT[F, L, ?]] {
      private def instance = implicitly[cats.effect.Sync[F]]

      override def pure[A](x: A): EitherT[F, L, A] =
        EitherT(instance.pure(\/-(x)))

      override def handleErrorWith[A](fa: EitherT[F, L, A])(f: Throwable => EitherT[F, L, A]): EitherT[F, L, A] =
        EitherT(instance.handleErrorWith(fa.run)(f.andThen(_.run)))

      override def raiseError[A](e: Throwable): EitherT[F, L, A] =
        EitherT(instance.raiseError(e))

      override def flatMap[A, B](fa: EitherT[F, L, A])(f: A => EitherT[F, L, B]): EitherT[F, L, B] =
        fa.flatMap(f)

      override def tailRecM[A, B](a: A)(f: A => EitherT[F, L, Either[A, B]]): EitherT[F, L, B] =
        EitherT(instance.tailRecM(a)(a0 => instance.map(f(a0).run) {
          case -\/(l) => Right(-\/(l))
          case \/-(Left(a1)) => Left(a1)
          case \/-(Right(b)) => Right(\/-(b))
        }))

      override def suspend[A](thunk: => EitherT[F, L, A]): EitherT[F, L, A] =
        EitherT(instance.suspend(thunk.run))

    }

}
