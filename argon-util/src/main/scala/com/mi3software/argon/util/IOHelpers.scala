package com.mi3software.argon.util

import scalaz.effect.IO
import scalaz._
import Scalaz._

object IOHelpers {

  def impureFunction[A, B](f: A => B): A => IO[B] = a => IO { f(a) }
  def recoverOption[A](value: IO[A]): IO[Option[A]] = value.map { _.point[Option] }.onException(None.point[IO])

  implicit val ioMonadError: MonadError[IO, Throwable] = new MonadError[IO, Throwable] {
    override def raiseError[A](e: Throwable): IO[A] = IO.throwIO(e)

    override def handleError[A](fa: IO[A])(f: Throwable => IO[A]): IO[A] =
      fa.except(f)

    override def point[A](a: => A): IO[A] =
      IO { a }

    override def bind[A, B](fa: IO[A])(f: A => IO[B]): IO[B] =
      fa.flatMap(f)
  }

}
