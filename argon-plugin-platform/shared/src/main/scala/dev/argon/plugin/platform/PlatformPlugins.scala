package dev.argon.plugin.platform

import dev.argon.plugin.PluginFactory
import dev.argon.plugin.PlatformPlugin
import dev.argon.plugin.PluginError
import zio.*

object PlatformPlugins {
  def pluginFactories: Map[String, PluginFactory] = Map(
    "source" -> dev.argon.plugins.source.SourcePluginFactory,
    "scheme" -> dev.argon.plugin.adapter.AdaptedPlatformPlugin.makeFactory([EX <: Throwable] => () =>
      dev.argon.plugins.scheme.SchemePlugin[EX]()
    ),
  )
}

