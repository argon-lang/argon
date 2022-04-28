package dev.argon.plugin

import zio.*
import zio.stream.*

import java.io.IOException

trait PluginLoader[E] {
  def load: IO[IOException, Seq[Plugin[E]]]
}

object PluginLoader extends PluginLoaderObjectPlatformSpecific
