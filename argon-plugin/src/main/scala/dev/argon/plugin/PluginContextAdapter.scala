package dev.argon.plugin

import dev.argon.compiler.*

trait PluginContextAdapter {
  val context: Context
  val plugin: Plugin[context.Error]
  
  def extractOptions(options: context.Options): plugin.Options[context.Env, context.Error]

  def extractExternMethodImplementation(impl: context.ExternMethodImplementation): plugin.ExternMethodImplementation
  def extractExternFunctionImplementation(impl: context.ExternFunctionImplementation): plugin.ExternFunctionImplementation
  def extractExternClassConstructorImplementation(impl: context.ExternClassConstructorImplementation): plugin.ExternClassConstructorImplementation
}

object PluginContextAdapter {
  type Aux[TContext <: Context, TPlugin <: Plugin[?]] = PluginContextAdapter {
    val context: TContext
    val plugin: TPlugin
  }
}
