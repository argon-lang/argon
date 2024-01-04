package dev.argon.plugin.platform

import dev.argon.plugin.Plugin
import dev.argon.plugins.source.{SourcePlugin, SourceError}

def plugins[R, E >: SourceError]: Map[String, Plugin[R, E]] = Map(
  "source" -> SourcePlugin[R, E](),
)

