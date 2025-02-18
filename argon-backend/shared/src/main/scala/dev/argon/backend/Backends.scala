package dev.argon.backend

import java.io.IOException

object Backends {
  def allBackends[E >: BackendException | IOException]: Seq[Backend[E]] = Seq(
    platforms.js.JSBackend()
  )
}
