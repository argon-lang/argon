package dev.argon.util

import scala.scalajs.js.JSON

trait StringHelpersPlatform {

  def escapeJSString(string: String): String = JSON.stringify(string)

}
