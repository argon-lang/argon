package dev.argon.backend

import dev.argon.backend.backends.js.{JSBackend, JSBackendFactory}

import java.io.IOException

object Backends {
  def allBackends[E >: BackendException | IOException]: Seq[Backend[E]] = Seq(
    JSBackend()
  )
  
  def allBackendFactories: Seq[BackendFactory] = Seq(
    JSBackendFactory
  )
}
