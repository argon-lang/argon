package com.mi3software.argon.compiler

import scalaz.{NonEmptyList, \/}
import scalaz.effect.IO

trait IOCompilation extends Compilation[IO] {
  def getMessages: IO[Vector[CompilationMessageNonFatal]]
  def getResult[A](fa: IO[A]): IO[NonEmptyList[CompilationError] \/ A]
}

object IOCompilation {

  final case class CompilationErrorException(errors: NonEmptyList[CompilationError]) extends Exception

  val compilationInstance: IO[IOCompilation] = for {
    messageAccum <- IO.newIORef(Vector.empty[CompilationMessageNonFatal])
  } yield new IOCompilation {
    override def diagnostic[A](value: A, messages: Vector[CompilationMessageNonFatal]): IO[A] = for {
      _ <- messageAccum.mod { acc => acc ++ messages }
    } yield value

    override def forErrors[A](errors: NonEmptyList[CompilationError], messages: Vector[CompilationMessageNonFatal]): IO[A] =
      messageAccum.mod { acc => acc ++ messages }.flatMap { _ => IO.throwIO(CompilationErrorException(errors)) }

    override def point[A](a: => A): IO[A] = IO { a }

    override def bind[A, B](fa: IO[A])(f: A => IO[B]): IO[B] =
      fa.flatMap(f)

    override def getMessages: IO[Vector[CompilationMessageNonFatal]] = messageAccum.read

    override def getResult[A](fa: IO[A]): IO[NonEmptyList[CompilationError] \/ A] =
      fa.catchSomeLeft {
        case CompilationErrorException(errors) => Some(errors)
        case _ => None
      }
  }
}
