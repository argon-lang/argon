package dev.argon.util

import zio.*
import zio.stream.*

final class ErrorAsThrowable[E] {

  final case class WrappedError(cause: Cause[E]) extends Throwable

  def wrap[R, A](z: ZIO[R, E, A]): ZIO[R, WrappedError, A] =
    z.catchAllCause { cause => ZIO.fail(WrappedError(cause)) }

  def wrap[R, A](z: ZStream[R, E, A]): ZStream[R, WrappedError, A] =
    z.catchAllCause { cause => ZStream.fail(WrappedError(cause)) }

  def unwrap[R, A](z: ZIO[R, WrappedError, A]): ZIO[R, E, A] =
    z.catchAll { err => ZIO.refailCause(err.cause) }

  def unwrap[R, A](z: ZStream[R, WrappedError, A]): ZStream[R, E, A] =
    z.catchAll { err => ZStream.failCause(err.cause) }

  def refineThrowable[R, A](z: ZIO[R, Throwable, A]): ZIO[R, E, A] =
    z.catchAll {
      case ex: WrappedError => ZIO.refailCause(ex.cause)
      case ex => ZIO.die(ex)
    }

  def refineThrowable[R, A](z: ZStream[R, Throwable, A]): ZStream[R, E, A] =
    z.catchAll {
      case ex: WrappedError => ZStream.failCause(ex.cause)
      case ex => ZStream.die(ex)
    }
}
