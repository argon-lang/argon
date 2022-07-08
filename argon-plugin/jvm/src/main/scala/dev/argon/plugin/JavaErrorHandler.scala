package dev.argon.plugin

import dev.argon.util.ErrorWrapper
import scala.reflect.TypeTest
import zio.*
import zio.stream.*
import java.io.IOException
import dev.argon.options.OptionsError
import dev.argon.plugin.{api => japi}

private[plugin] object JavaErrorHandler {
  def handleErrors[E >: IOException, EX <: Exception](ex: Throwable)(using TypeTest[Throwable, EX], ErrorWrapper[E, EX]): IO[E, Nothing] =
    ex match {
      case ex: EX => ZIO.failCause(summon[ErrorWrapper[E, EX]].unwrap(ex))
      case ex: IOException => ZIO.fail(ex)
      case ex => ZIO.die(ex)
    }

  def handleErrorsStream[E >: IOException, EX <: Exception](ex: Throwable)(using TypeTest[Throwable, EX], ErrorWrapper[E, EX]): Stream[E, Nothing] =
    ZStream.fromZIO(handleErrors(ex))

  def handleOptionsExceptions: PartialFunction[Throwable, OptionsError.ParseError] = {
    case ex: japi.options.DuplicateOptionValueException => OptionsError.MultipleValuesNotSupported(ex.optionName)
    case ex: japi.options.InvalidOptionValueException => OptionsError.CouldNotDecode(ex.optionName)
  }
}