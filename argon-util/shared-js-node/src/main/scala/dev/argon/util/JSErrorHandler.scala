package dev.argon.util

import scala.scalajs.js.JavaScriptException

object JSErrorHandler {

  def handleJSError(error: Any): Throwable =
    error match {
      case ex: Throwable => ex
      case ex => JavaScriptException(ex)
    }

}
