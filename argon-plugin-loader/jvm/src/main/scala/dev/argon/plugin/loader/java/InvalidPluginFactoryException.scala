package dev.argon.plugin.loader.java

final class InvalidPluginFactoryException()
  extends JavaPluginLoadException("Specified plugin factory does not implement PluginFactory", null)
