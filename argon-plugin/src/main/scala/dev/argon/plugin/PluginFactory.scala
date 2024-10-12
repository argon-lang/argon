package dev.argon.plugin

import zio.*

sealed trait PluginFactory

object PluginFactory {
  trait OfPlatform extends PluginFactory {
    def create[E >: PluginError]: UIO[PlatformPlugin[E]]
  }

  trait OfFormat extends PluginFactory {
    def create[E >: PluginError](platforms: PlatformPluginSet[E]): UIO[FormatPlugin[E, platforms.type]]
  }
}

