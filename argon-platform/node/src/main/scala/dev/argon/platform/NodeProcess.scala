package dev.argon.platform

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("node:process", JSImport.Default)
private[platform] object NodeProcess extends js.Object {
  val env: js.Dictionary[String] = js.native
  val argv: js.Array[String] = js.native
  var exitCode: js.UndefOr[Int | String | Null] = js.native
}
