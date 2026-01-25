package dev.argon.backend.sjs

import dev.argon.backend.sjs.metadata.BackendMetadata
import nobleidl.sjs.core.ErrorChecker

import scala.scalajs.js

trait SimpleBackendFactory extends js.Object {
  def create[E, A](errorChecker: ErrorChecker[E], hostOperations: HostOperations[E], f: js.Function1[Backend[E, ?], A]): A
}

trait BackendFactory extends SimpleBackendFactory {
  def metadata: BackendMetadata
  def close(): js.Promise[Unit]
}
