package dev.argon.util

import dev.argon.util.async.ErrorWrapper
import scala.reflect.TypeTest
import zio.Cause

class WrappedErrorContext[E] {
  final case class Error(error: Cause[E]) extends Exception
  object Error {
    given ErrorWrapper[E]:
      override type EX = Error
      override def exceptionTypeTest: TypeTest[Throwable, Error] = summon

      override def wrap(error: Cause[E]): EX = Error(error)

      override def unwrap(ex: Error): Cause[E] = ex.error
    end given
  }
}
