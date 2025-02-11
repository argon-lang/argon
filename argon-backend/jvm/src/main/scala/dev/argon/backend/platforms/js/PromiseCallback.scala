package dev.argon.backend.platforms.js

import org.graalvm.polyglot.Value

@FunctionalInterface
trait PromiseCallback {
  def invoke(resolve: Value, reject: Value): Unit
}
