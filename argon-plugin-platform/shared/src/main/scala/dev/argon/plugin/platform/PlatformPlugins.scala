package dev.argon.plugin.platform

import dev.argon.plugin.PluginFactory

object PlatformPlugins {
  def pluginFactories: Map[String, PluginFactory] = Map(
    "source" -> dev.argon.plugins.source.SourcePluginFactory,
    "lua" -> dev.argon.plugins.lua.LuaPluginFactory
  )
}

