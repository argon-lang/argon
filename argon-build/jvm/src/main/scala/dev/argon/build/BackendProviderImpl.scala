package dev.argon.build

import dev.argon.backend.js.JSBackend
import dev.argon.backend.module.ArModuleBackend
import dev.argon.backend.Backend
import zio.{ULayer, ZLayer}
import cats.implicits._

object BackendProviderImpl {

  def live: ULayer[BackendProvider] = ZLayer.succeed(
    new BackendProvider.Service {

      val allBackends = Vector(ArModuleBackend, JSBackend)

      override def findBackend(id: String): Option[Backend] =
        allBackends.find { _.id === id }
    }
  )

}
