package dev.argon.driver.scalaApi.command

import nobleidl.core.JavaAdapter

import java.nio.file.Path

trait CommandFilePathPlatformSpecific {
  def javaAdapter(): JavaAdapter[CommandFilePath, Path] =
    JavaAdapter.identity
}
