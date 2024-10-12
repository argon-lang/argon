package dev.argon.plugins.source

import dev.argon.plugin.*
import zio.*

object SourcePluginFactory extends PluginFactory.OfFormat {
  override def create[E >: PluginError](platforms: PlatformPluginSet[E]): UIO[FormatPlugin[E, platforms.type]] =
    ZIO.succeed(new SourcePlugin[E, platforms.type](platforms))
}
