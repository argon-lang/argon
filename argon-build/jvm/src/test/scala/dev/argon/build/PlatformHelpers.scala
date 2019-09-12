package dev.argon.build

import dev.argon.io.IOEnvironment
import zio.blocking.Blocking
import zio.console.Console
import zio.system.System

object PlatformHelpers {
  def ioEnvironment: Blocking with Console with System => IOEnvironment = new IOEnvironment(_)
}
