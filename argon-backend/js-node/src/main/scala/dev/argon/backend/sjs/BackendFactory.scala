package dev.argon.backend.sjs

import nobleidl.sjs.core.ErrorChecker

import scala.scalajs.js

trait BackendFactory extends js.Object {
  def create[E, A](errorChecker: ErrorChecker[E], hostOperations: HostOperations[E], f: js.Function1[Backend[E, ?, ?], A]): A
}
