package com.mi3software.argon.util

import scalaz.effect.IO

object IOHelpers {

  def impureFunction[A, B](f: A => B): A => IO[B] = a => IO { f(a) }

}
