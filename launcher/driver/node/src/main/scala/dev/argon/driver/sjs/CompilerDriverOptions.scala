package dev.argon.driver.sjs

import dev.argon.backend.sjs.{BackendFactory, BinaryResource, DirectoryResource, BinaryResourceSink, DirectoryResourceSink}
import dev.argon.driver.sjs.command.DriverCommand
import dev.argon.io.PathLike

import scala.scalajs.js

trait CompilerDriverOptions extends js.Object {
  def backendFactories: js.Array[BackendFactory]
  def command: DriverCommand[BinaryResource[js.Error], DirectoryResource[js.Error], BinaryResourceSink[js.Error], DirectoryResourceSink[js.Error]]
}
