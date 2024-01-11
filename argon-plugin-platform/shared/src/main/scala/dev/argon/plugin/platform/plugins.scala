package dev.argon.plugin.platform

import dev.argon.plugin.*
import dev.argon.plugins.source.{SourcePluginFactory, SourceEnv, SourceError}

type AnyPluginEnv = SourceEnv
type AnyPluginError = SourceError

def pluginFactories[R, E >: SourceError]: Map[String, PluginFactory[R, E]] = Map(
  "source" -> SourcePluginFactory[R, E](),
)

