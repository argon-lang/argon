package dev.argon.plugins.source

import dev.argon.plugin.*
import zio.*

object SourcePluginFactory extends PluginFactory.OfFormat {
  override def create(platforms: PlatformPluginSet): UIO[FormatPlugin[platforms.type]] =
    ZIO.succeed(new SourcePlugin[platforms.type](platforms))
}
