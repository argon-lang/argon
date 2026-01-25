package dev.argon.driver.scalaApi.command

import dev.argon.io.PathLike
import nobleidl.core.JSAdapter

trait CommandFilePathPlatformSpecific {
  def jsAdapter(): JSAdapter[CommandFilePath, PathLike] =
    JSAdapter.identity
}
