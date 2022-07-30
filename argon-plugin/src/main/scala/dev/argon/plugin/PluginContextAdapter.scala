package dev.argon.plugin

import dev.argon.compiler.*

trait PluginContextAdapter {
  val context: Context
  val plugin: Plugin[context.Env, context.Error]

  def extractExternMethodImplementation(impl: context.ExternMethodImplementation): plugin.ExternalMethodImplementation
}

object PluginContextAdapter {
  type Aux[TContext <: Context, TPlugin <: Plugin[?, ?]] = PluginContextAdapter {
    val context: TContext
    val plugin: TPlugin
  }
}
