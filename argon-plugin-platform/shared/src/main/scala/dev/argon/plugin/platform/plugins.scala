package dev.argon.plugin.platform

import dev.argon.plugin.*
import dev.argon.plugins

def pluginFactories: Map[String, PluginFactory] = Map(
  "source" -> plugins.source.SourcePluginFactory,
  "lua" -> plugins.lua.LuaPluginFactory
)

