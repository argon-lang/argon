package dev.argon.util

import org.apache.commons.text.StringEscapeUtils

trait StringHelpersPlatform {

  def escapeJSString(string: String): String = "\"" + StringEscapeUtils.escapeEcmaScript(string) + "\""

}
