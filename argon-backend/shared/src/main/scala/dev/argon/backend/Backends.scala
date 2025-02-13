package dev.argon.backend

object Backends {
  def allBackends: Seq[Backend] = Seq(
    platforms.js.JSBackend()
  )
}
