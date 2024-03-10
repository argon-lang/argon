package dev.argon.plugin

open class PluginException(val pluginId: String, message: String | Null, cause: Throwable | Null) extends Exception(message, cause) {
  def this(pluginId: String, message: String | Null) = this(pluginId, message, null)
  def this(pluginId: String, cause: Throwable | Null) = this(pluginId, null, cause)
  def this(pluginId: String) = this(pluginId, null, null)
}
