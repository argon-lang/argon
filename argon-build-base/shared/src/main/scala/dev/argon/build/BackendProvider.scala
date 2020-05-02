package dev.argon.build

import dev.argon.backend.Backend

object BackendProvider {

  trait Service {
    def findBackend(id: String): Option[Backend]
  }

}
