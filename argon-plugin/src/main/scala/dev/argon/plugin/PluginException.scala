package dev.argon.plugin

class PluginException(message: String, cause: Throwable) extends Exception(message, cause) {
  def this(message: String) = this(message, null)
}


