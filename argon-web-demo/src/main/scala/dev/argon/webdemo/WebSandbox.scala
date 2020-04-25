package dev.argon.webdemo

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
trait WebSandbox extends js.Object {
  val promise: js.Promise[Unit]
  def run(code: String): js.Promise[Unit]
  def destroy(): Unit
}

@js.native
@JSImport("websandbox", JSImport.Default)
object WebSandbox extends js.Object {

  def create(api: js.Any): WebSandbox = js.native

}