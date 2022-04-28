package dev.argon.plugin

import zio.Cause
import dev.argon.compiler.*
import dev.argon.util.ErrorWrapper

final case class JavaCompErrorWrapper(error: Cause[CompError]) extends Exception

given ErrorWrapper[CompError, JavaCompErrorWrapper] with
  override def wrap(error: Cause[CompError]): JavaCompErrorWrapper = JavaCompErrorWrapper(error)
  def unwrap(ex: JavaCompErrorWrapper): Cause[CompError] = ex.error
end given

