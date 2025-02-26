package dev.argon.backend

import java.io.IOException

object Backends {
  lazy val allBackendFactories: Seq[BackendFactory] = Seq(
    backends.js.JSBackendFactory
  )
}
