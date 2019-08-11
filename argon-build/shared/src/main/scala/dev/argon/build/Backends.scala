package dev.argon.build

import dev.argon.compiler.backend.Backend
import dev.argon.backend.js.JSBackend
import dev.argon.backend.module.ArModuleBackend
import cats._
import cats.implicits._

object Backends {

  val allBackends = Vector(ArModuleBackend, JSBackend)

  def find(id: String): Option[Backend] =
    allBackends.find { _.id === id }

}
