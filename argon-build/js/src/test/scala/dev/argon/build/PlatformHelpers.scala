package dev.argon.build

import dev.argon.io.NodeIOEnvironment
import zio.console.Console
import zio.system.System

object PlatformHelpers {
  def ioEnvironment: Console with System => NodeIOEnvironment = new NodeIOEnvironment(_)
}
