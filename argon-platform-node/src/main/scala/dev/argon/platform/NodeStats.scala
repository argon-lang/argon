package dev.argon.platform

import scala.scalajs.js

@js.native
private[platform] trait NodeStats extends js.Object {

  def isDirectory(): Boolean = js.native

}
