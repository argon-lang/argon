package dev.argon.backend

import dev.argon.backend.metadata.BackendMetadata
import zio.*

import java.io.IOException

trait BackendFactory {
  val metadata: BackendMetadata  

  def load[E >: BackendException | IOException]: ZIO[Scope, E, Backend[E]]
}
