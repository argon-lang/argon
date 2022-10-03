package dev.argon.vm.engine

import dev.argon.vm.format.Chunk
import zio.*

trait Engine {
  def execute(): Task[Unit]
}

object Engine extends EnginePlatformObject
