package dev.argon.driver

import scala.scalajs.js
import scala.scalajs.js.annotation.*

trait CompilerDriverInterface extends js.Object {
  def runCommand(options: CompilerDriverJSOptions): js.Promise[Int]
}
