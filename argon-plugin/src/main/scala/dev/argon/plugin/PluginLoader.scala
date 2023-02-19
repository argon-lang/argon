package dev.argon.plugin

import zio.*
import dev.argon.util.xml
import dev.argon.io.PathLike

trait PluginLoader[R, E] {
  type PluginConfig
  val configTagName: String
  def decodeConfig(path: PathLike, elem: xml.Element): Either[String, PluginConfig]

  def load(config: PluginConfig): ZIO[R & Scope, E, Plugin[R, E]]
}
