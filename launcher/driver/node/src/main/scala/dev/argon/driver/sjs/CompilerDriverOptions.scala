package dev.argon.driver.sjs

import dev.argon.backend.sjs.BackendFactory
import dev.argon.driver.sjs.command.DriverCommand

import scala.scalajs.js

trait CompilerDriverOptions extends js.Object {
  def backendFactories: js.Array[BackendFactory]
  def command: DriverCommand
}
