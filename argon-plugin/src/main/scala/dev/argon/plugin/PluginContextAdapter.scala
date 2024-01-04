package dev.argon.plugin

import dev.argon.compiler.*

trait PluginContextAdapter {
  val context: Context
  val plugin: Plugin[context.Env, context.Error]
  
  def extractOptions(options: context.Options): plugin.Options

  def extractExternMethodImplementation(impl: context.ExternMethodImplementation): plugin.ExternMethodImplementation
  def extractExternFunctionImplementation(impl: context.ExternFunctionImplementation): plugin.ExternFunctionImplementation
  def extractExternClassConstructorImplementation(impl: context.ExternClassConstructorImplementation): plugin.ExternClassConstructorImplementation
}

object PluginContextAdapter {
  type Aux[TContext <: Context, TPlugin <: Plugin[?, ?]] = PluginContextAdapter {
    val context: TContext
    val plugin: TPlugin
  }

  def apply[R, E >: CompError](p: Plugin[R, E], c: PluginContext[R, E] { val plugin: p.type }): PluginContextAdapter.Aux[c.type, p.type] =
    new PluginContextAdapter {
      override val context: c.type = c
      override val plugin: p.type = p

      override def extractOptions(options: context.Options): plugin.Options =
        options

      override def extractExternMethodImplementation(impl: context.ExternMethodImplementation): plugin.ExternMethodImplementation =
        impl

      override def extractExternFunctionImplementation(impl: context.ExternFunctionImplementation): plugin.ExternFunctionImplementation =
        impl

      override def extractExternClassConstructorImplementation(impl: context.ExternClassConstructorImplementation): plugin.ExternClassConstructorImplementation =
        impl
    }
}
