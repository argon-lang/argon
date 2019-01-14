package com.mi3software.argon.util

import scalaz._
import Scalaz._

object MonadErrorExtensions {
  implicit class MonadErrorHelpers[F[_], A](val fa: F[A]) extends AnyVal {

    def ensuring[B, S](sequel: F[B])(implicit monadErrorInstance: MonadError[F, S]): F[A] =
      monadErrorInstance.handleError(fa) { s => sequel.flatMap { _ => monadErrorInstance.raiseError(s) } }
      .flatMap { a =>
        sequel.map { _ => a }
      }

  }
}
