package dev.argon.driver.sjs.command

import dev.argon.driver.scalaApi.command.CommandFilePathPlatformSpecific
import dev.argon.io.PathLike
import nobleidl.core.JSAdapter

type CommandFilePath = PathLike

object CommandFilePath {
  def jsAdapter(): JSAdapter[CommandFilePath, PathLike] =
    JSAdapter.identity
}
