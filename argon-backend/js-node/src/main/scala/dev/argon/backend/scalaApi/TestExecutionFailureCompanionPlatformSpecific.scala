package dev.argon.backend.scalaApi

import nobleidl.core.JSAdapter

import scala.scalajs.js.JavaScriptException

trait TestExecutionFailureCompanionPlatformSpecific {
  def jsAdapter(): JSAdapter[Throwable, Any] =
    new JSAdapter[Throwable, Any] {
      override def toJS(s: Throwable): Any =
        s match {
          case JavaScriptException(value) => value
          case _ => s
        }

      override def fromJS(j: Any): Throwable =
        j.asInstanceOf[Matchable] match {
          case ex: Throwable => ex
          case _ => JavaScriptException(j)
        }
    }
}
