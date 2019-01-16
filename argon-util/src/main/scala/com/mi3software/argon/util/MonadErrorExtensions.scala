package com.mi3software.argon.util

import scalaz._
import Scalaz._
import scalaz.zio.{IO, ZIO}

object MonadErrorExtensions {
  implicit class MonadErrorHelpers[F[_, _], E, A](val fa: F[E, A]) extends AnyVal {

    def ensuring[B](sequel: F[E, B])(implicit monadErrorInstance: MonadError[F[E, ?], E]): F[E, A] =
      monadErrorInstance.handleError(fa) { s =>
        monadErrorInstance.handleError(sequel.map { _ => }) { _ => ().point[F[E, ?]] }
          .flatMap { _ => monadErrorInstance.raiseError(s) }
      }
        .flatMap { a =>
          sequel.map { _ => a }
        }

  }

  implicit class MonadErrorThrowableHelpers[F[_, _], A](val fa: F[Throwable, A]) extends AnyVal {

    def closing(resource: AutoCloseable)(implicit zio: ZIO[F], monadErrorInstance: MonadError[F[Throwable, ?], Throwable]): F[Throwable, A] =
      fa.ensuring(zio.liftZIO(IO.syncThrowable { resource.close() }))

  }

}
