package dev.argon.plugin

class PluginXmlException(message: String) extends PluginException(message)

class PluginIdMissingException extends PluginXmlException("Plugin ID not specified")
class PluginNameMissingException extends PluginXmlException("Plugin name not specified")
class PluginVersionMissingException extends PluginXmlException("Plugin version not specified")
class PluginAPIVersionInvalidException extends PluginXmlException("Plugin API version is invalid")
class PluginOptionInvalidException(message: String) extends PluginXmlException(message)
