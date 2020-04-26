package dev.argon.compiler

import cats._
import cats.implicits._
import cats.data.NonEmptyList
import zio.IO

object Compilation {

  def forErrors[A](errors: NonEmptyList[CompilationError], messages: Vector[CompilationMessageNonFatal] = Vector()): Comp[A] =
    IO.fail(errors)

  def forErrors[A](head: CompilationError, tail: CompilationError*): Comp[A] =
    forErrors(NonEmptyList(head, tail.toList))

  def require(value: Boolean)(head: CompilationError, tail: CompilationError*): Comp[Unit] =
    if(value) IO.unit
    else forErrors(head, tail: _*)

  final def requireSome[A](option: Option[A])(head: CompilationError, tail: CompilationError*): Comp[A] =
    option match {
      case Some(a) => IO.succeed(a)
      case None => forErrors(head, tail: _*)
    }

}

