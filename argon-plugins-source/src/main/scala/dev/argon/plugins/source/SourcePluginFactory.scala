package dev.argon.plugins.source

import dev.argon.plugin.{CompositePlugin, CompositePluginFactory, PlatformPluginSet}
import dev.argon.plugin.Plugin.WithId
import zio.{Scope, ZIO}

final class SourcePluginFactory[R <: SourceEnv, E >: SourceError] extends CompositePluginFactory[R, E] {
  val pluginId: "source" = "source"
  
  override def make(platformPlugins: PlatformPluginSet[R, E]): ZIO[R, E, CompositePlugin[R, E, platformPlugins.type] & WithId["source"]] =
    ZIO.succeed(SourcePlugin[R, E, platformPlugins.type](platformPlugins))
}
