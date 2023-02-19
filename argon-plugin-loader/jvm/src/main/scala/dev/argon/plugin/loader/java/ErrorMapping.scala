package dev.argon.plugin.loader.java

import zio.*
import zio.stream.*

import scala.reflect.TypeTest

trait ErrorMapping[E, EX <: Throwable] {
  given typeTest: TypeTest[Throwable, EX]

  def wrap(error: Cause[E]): EX
  def unwrap(ex: EX): Cause[E]

  final def refine[EX2 >: EX <: Throwable](ex: EX2): IO[E, Nothing] =
    ex match {
      case ex: EX => ZIO.failCause(unwrap(ex))
      case _ => ZIO.die(ex)
    }

  final def refineStream[EX2 >: EX <: Throwable](ex: EX2): Stream[E, Nothing] =
    ZStream.fromZIO(refine(ex))
}
