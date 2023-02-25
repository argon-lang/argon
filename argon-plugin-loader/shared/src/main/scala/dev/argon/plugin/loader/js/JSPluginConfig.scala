package dev.argon.plugin.loader.js

import dev.argon.io.PathLike

final case class JSPluginConfig
(
  module: PathLike,
  exportName: String,
)
