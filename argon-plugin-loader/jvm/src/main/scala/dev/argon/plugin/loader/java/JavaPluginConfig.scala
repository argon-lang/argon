package dev.argon.plugin.loader.java

import java.nio.file.Path

final case class JavaPluginConfig
(
  pluginFactoryClass: String,
  modules: Seq[Path],
)
