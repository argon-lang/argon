package com.mi3software.argon.util

import scalaz._
import Scalaz._
import scalaz.zio._

object IOHelpers {

  def impureFunction[A, B](f: A => B): A => IO[Throwable, B] = a => IO.syncThrowable { f(a) }

}
