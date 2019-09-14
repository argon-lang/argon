package dev.argon

import zio.blocking.Blocking
import zio.console.Console
import zio.system.System

import dev.argon.io.IOEnvironment

object PlatformHelpers {
  def ioEnvironment: Blocking with Console with System => IOEnvironment = new IOEnvironment(_)

  def getCommandLineArgs(args: List[String]): List[String] =
    args
}
