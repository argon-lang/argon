package dev.argon.plugin

import dev.argon.compiler.{CompEnv, CompError}


trait PluginWithAdapter[R <: CompEnv, E >: CompError, TPlugin <: Plugin[R, E]] {
  val plugin: Plugin[R, E]
  val adapter: PluginAdapter[R, E, TPlugin, plugin.type]
}
