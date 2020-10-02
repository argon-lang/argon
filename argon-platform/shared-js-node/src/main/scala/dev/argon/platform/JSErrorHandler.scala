package dev.argon.platform

import dev.argon.io.JSIOException

import scala.scalajs.js

trait JSErrorHandler {

  protected def handleJSError[E](error: Any): Throwable =
    error match {
      case ex: Throwable => ex
      case error: js.Error => JSIOException(error)
      case msg: String => JSIOException(js.Error(msg))
      case _ => JSIOException(js.Error("An unknown error occurred"))
    }

}
