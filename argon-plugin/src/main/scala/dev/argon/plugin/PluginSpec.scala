package dev.argon.plugin

import scala.xml

final case class PluginSpec
(
  info: PluginSpec.Info,
  config: Seq[xml.Node],
)

object PluginSpec {
  final case class Info
  (
    name: String,
    apiVersion: Int,
  )
}
