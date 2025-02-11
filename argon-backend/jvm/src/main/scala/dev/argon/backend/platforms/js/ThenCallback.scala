package dev.argon.backend.platforms.js

import org.graalvm.polyglot.Value

@FunctionalInterface
trait ThenCallback {
  def invoke(value: Value): Unit
}
