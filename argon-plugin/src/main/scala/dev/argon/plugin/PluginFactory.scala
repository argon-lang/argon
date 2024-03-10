package dev.argon.plugin

import zio.*

sealed trait PluginFactory

object PluginFactory {
  trait OfPlatform extends PluginFactory {
    def create: UIO[PlatformPlugin]
  }

  trait OfFormat extends PluginFactory {
    def create(platforms: PlatformPluginSet): UIO[FormatPlugin[platforms.type]]
  }
}

