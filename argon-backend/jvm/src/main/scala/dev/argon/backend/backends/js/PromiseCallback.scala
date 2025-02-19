package dev.argon.backend.backends.js

import org.graalvm.polyglot.Value

@FunctionalInterface
trait PromiseCallback {
  def invoke(resolve: Value, reject: Value): Unit
}
