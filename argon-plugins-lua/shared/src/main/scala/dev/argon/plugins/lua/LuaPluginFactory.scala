package dev.argon.plugins.lua

import dev.argon.plugin.*
import zio.*

object LuaPluginFactory extends PluginFactory.OfPlatform {
  override def create: UIO[PlatformPlugin] =
    ZIO.succeed(new LuaPlugin())
}
