package dev.argon.plugin.loader.java

final class PluginFactoryNotFoundException(className: String, ex: Throwable)
  extends JavaPluginLoadException("Could not find class: " + className, ex)
