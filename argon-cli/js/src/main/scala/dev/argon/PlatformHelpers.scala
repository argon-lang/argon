package dev.argon

import zio.console.Console
import zio.system.System

import dev.argon.io.NodeIOEnvironment

object PlatformHelpers {
  def ioEnvironment: Console with System => NodeIOEnvironment = new NodeIOEnvironment(_)
}
