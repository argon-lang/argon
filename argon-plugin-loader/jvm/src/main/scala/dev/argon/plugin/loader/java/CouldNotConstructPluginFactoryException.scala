package dev.argon.plugin.loader.java

final class CouldNotConstructPluginFactoryException(ex: Throwable | Null)
  extends JavaPluginLoadException("Could not construct plugin factory.", ex)
