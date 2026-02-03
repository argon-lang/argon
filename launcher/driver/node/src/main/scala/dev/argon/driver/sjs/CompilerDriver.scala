package dev.argon.driver.sjs

import dev.argon.backend.sjs.metadata.BackendMetadata
import dev.argon.driver.sjs.command.DriverCommand

import scala.scalajs.js

trait CompilerDriver extends js.Object {
  def parseMetadata(metadata: String): BackendMetadata
  def parseCommandLineArguments(backends: js.Array[BackendMetadata], args: js.Array[String]): DriverCommand[String, String, String, String]
  def runCommand(options: CompilerDriverOptions): js.Promise[Int]
}
