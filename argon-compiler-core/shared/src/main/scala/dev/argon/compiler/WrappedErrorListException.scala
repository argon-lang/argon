package dev.argon.compiler

import java.io.IOException

import cats.data.NonEmptyList

final case class WrappedErrorListException(errorList: ErrorList) extends Exception

object WrappedErrorListException {

  def toThrowable(errorList: ErrorList): Throwable = WrappedErrorListException(errorList)
  def fromThrowable: PartialFunction[Throwable, ErrorList] = {
    case WrappedErrorListException(errorList) => errorList
    case ex: IOException => NonEmptyList.of(CompilationError.ResourceIOError(CompilationMessageSource.ThrownException(ex)))
  }

}
