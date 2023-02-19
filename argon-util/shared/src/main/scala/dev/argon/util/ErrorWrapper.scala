package dev.argon.util

import zio.*

import scala.reflect.TypeTest

trait ErrorWrapper[E, EX <: Throwable] {
  def wrap(error: Cause[E]): EX
  def unwrap(ex: EX): Cause[E]
}

object ErrorWrapper {
  def apply[E, EX <: Throwable](using errorWrapper: ErrorWrapper[E, EX]): ErrorWrapper[E, EX] =
    errorWrapper

  def wrapEffect[R, E, EX <: Throwable, A](a: ZIO[R, E, A])(using ErrorWrapper[E, EX]): ZIO[R, EX, A] =
    a.mapErrorCause { cause => Cause.fail(ErrorWrapper[E, EX].wrap(cause)) }

  def unwrapEffect[R, E, EX <: Throwable, A](a: ZIO[R, Throwable, A])(using ErrorWrapper[E, EX], TypeTest[Throwable, EX]): ZIO[R, E, A] =
    a.catchAll {
      case ex: EX => ZIO.failCause(ErrorWrapper[E, EX].unwrap(ex))
      case ex => ZIO.die(ex)
    }
}

