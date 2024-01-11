package dev.argon.plugin

import dev.argon.compiler.{CompEnv, CompError}
import zio.*

sealed trait PluginFactory[R, E] {
  val pluginId: String
}
abstract class PlatformPluginFactory[R <: CompEnv, E >: CompError] extends PluginFactory[R, E] {
  def make: ZIO[R, E, PlatformPlugin[R, E] & Plugin.WithId[pluginId.type]]
}
abstract class CompositePluginFactory[R <: CompEnv, E >: CompError] extends PluginFactory[R, E] {
  def make(platformPlugins: PlatformPluginSet[R, E]): ZIO[R, E, CompositePlugin[R, E, platformPlugins.type] & Plugin.WithId[pluginId.type]]
}

