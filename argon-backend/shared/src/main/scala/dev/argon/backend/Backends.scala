package dev.argon.backend

import java.io.IOException

object Backends {
  def allBackendFactories: Seq[BackendFactory] = Seq(
    backends.js.JSBackendFactory
  )
}
