package com.mi3software.argon.util

import scalaz.effect.IO
import scalaz._
import Scalaz._

object IOHelpers {

  def impureFunction[A, B](f: A => B): A => IO[B] = a => IO { f(a) }
  def recoverOption[A](value: IO[A]): IO[Option[A]] = value.map { _.point[Option] }.onException(None.point[IO])

}
