package dev.argon.compiler

import java.io.IOException

import cats._
import cats.implicits._
import cats.data.NonEmptyList
import zio._

object Compilation {

  def forErrors[A](errors: NonEmptyChunk[CompilationError], messages: Vector[CompilationMessageNonFatal] = Vector()): Comp[A] =
    IO.halt(errors.reduceMapLeft(Cause.fail(_))((cause, compError) => Cause.Both(cause, Cause.fail(compError))))

  def forErrors[A](head: CompilationError, tail: CompilationError*): Comp[A] =
    forErrors(NonEmptyChunk(head, tail: _*))

  def require(value: Boolean)(head: CompilationError, tail: CompilationError*): Comp[Unit] =
    if(value) IO.unit
    else forErrors(head, tail: _*)

  def requireM(value: Comp[Boolean])(head: CompilationError, tail: CompilationError*): Comp[Unit] =
    value.flatMap(require(_)(head, tail: _*))

  final def requireSome[A](option: Option[A])(head: CompilationError, tail: CompilationError*): Comp[A] =
    option match {
      case Some(a) => IO.succeed(a)
      case None => forErrors(head, tail: _*)
    }

  def errorForIOException(ex: IOException): CompError =
    CompilationError.ResourceIOError(CompilationMessageSource.ThrownException(ex))

  def forIOException(ex: IOException): Comp[Nothing] =
    IO.fail(errorForIOException(ex))

}

