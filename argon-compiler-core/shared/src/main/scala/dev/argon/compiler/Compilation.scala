package dev.argon.compiler

import java.io.IOException

import zio._
import zio.stream._

object Compilation {

  def forErrors[A](errors: NonEmptyChunk[CompilationError]): Comp[A] =
    IO.halt(errors.reduceMapLeft(Cause.fail(_))((cause, compError) => Cause.Both(cause, Cause.fail(compError))))

  def forErrors[A](head: CompilationError): Comp[A] =
    forErrors(NonEmptyChunk(head))

  def require(value: Boolean)(head: CompilationError): Comp[Unit] =
    if(value) IO.unit
    else forErrors(head)

  def requireM(value: Comp[Boolean])(head: CompilationError): Comp[Unit] =
    value.flatMap(require(_)(head))

  final def requireSome[A](option: Option[A])(head: CompilationError): Comp[A] =
    option match {
      case Some(a) => IO.succeed(a)
      case None => forErrors(head)
    }

  def errorForIOException(ex: IOException): CompilationError =
    DiagnosticError.ResourceIOError(DiagnosticSource.ThrownException(ex))

  def forIOException(ex: IOException): Comp[Nothing] =
    IO.fail(errorForIOException(ex))


  private def isCorrectCauseType(cause: Cause[Any]): Boolean =
    cause.fold(
      empty = true,
      failCase = {
        case _: CompilationError => true
        case _ => false
      },
      dieCase = _ => true,
      interruptCase = _ => true,
    )(
      thenCase = _ && _,
      bothCase = _ && _,
      tracedCase = (a, _) => a,
    )

  def unwrapThrowableCause(ex: Throwable): Cause[CompilationError] =
    ex match {
      case ex: CompilationError => Cause.fail(ex)
      case ex: IOException => Cause.fail(Compilation.errorForIOException(ex))
      case FiberFailure(cause) if isCorrectCauseType(cause) =>
        cause.flatMap {
          case error: CompilationError => Cause.fail(error)
          case _ => Cause.empty
        }

      case _ => Cause.die(ex)
    }

  def unwrapThrowable(ex: Throwable): IO[CompilationError, Nothing] =
    IO.halt(unwrapThrowableCause(ex))

  def unwrapThrowableManaged(ex: Throwable): Managed[CompilationError, Nothing] =
    Managed.halt(unwrapThrowableCause(ex))

  def unwrapThrowableStream(ex: Throwable): Stream[CompilationError, Nothing] =
    Stream.halt(unwrapThrowableCause(ex))

}

