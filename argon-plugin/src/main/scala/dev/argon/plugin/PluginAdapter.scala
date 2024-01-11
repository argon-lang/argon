package dev.argon.plugin

import dev.argon.compiler.{CompEnv, CompError}

trait PluginAdapter[R <: CompEnv, E >: CompError, CtxPlugin <: Plugin[R, E], TPlugin <: Plugin[R, E]] {
  val ctxPlugin: CtxPlugin
  val plugin: TPlugin

  def getExternMethodImplementation(method: ctxPlugin.externMethod.Implementation): plugin.externMethod.Implementation
  def getExternMethodReference(method: ctxPlugin.externMethod.Reference): plugin.externMethod.Reference
  def getExternFunctionImplementation(func: ctxPlugin.externFunction.Implementation): plugin.externFunction.Implementation
  def getExternFunctionReference(func: ctxPlugin.externFunction.Reference): plugin.externFunction.Reference
  def getExternClassConstructorImplementation(ctor: ctxPlugin.externClassConstructor.Implementation): plugin.externClassConstructor.Implementation
  def getExternClassConstructorReference(ctor: ctxPlugin.externClassConstructor.Reference): plugin.externClassConstructor.Reference
}
