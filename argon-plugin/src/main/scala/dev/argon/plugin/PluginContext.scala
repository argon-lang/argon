package dev.argon.plugin

import dev.argon.compiler.{CompError, Context}
import dev.argon.options.OptionDecoder
import zio.*

sealed abstract class PluginContext[R, E >: CompError, P <: Plugin[R, E]] extends Context {
  val plugin: P

  override type Env = R
  override type Error = E

  override type Options = plugin.Options

  override type ExternMethodImplementation = plugin.externMethod.Implementation
  override type MethodReference = plugin.externMethod.Reference
  override type ExternFunctionImplementation = plugin.externFunction.Implementation
  override type FunctionReference = plugin.externFunction.Reference
  override type ExternClassConstructorImplementation = plugin.externClassConstructor.Implementation
  override type ClassConstructorReference = plugin.externClassConstructor.Reference

  override def getExternMethodImplementation(options: Options, id: String): ZIO[Env, Option[Error], ExternMethodImplementation] =
    plugin.loadExternMethod(options)(id).some

  override def getExternFunctionImplementation(options: Options, id: String): ZIO[Env, Option[Error], ExternFunctionImplementation] =
    plugin.loadExternFunction(options)(id).some

  override def getExternClassConstructorImplementation(options: Options, id: String): ZIO[Env, Option[Error], ExternClassConstructorImplementation] =
    plugin.loadExternClassConstructor(options)(id).some
}

object PluginContext {
  def apply[R, E >: CompError](p: Plugin[R, E]): PluginContext[R, E, p.type] =
    new PluginContext[R, E, p.type] {
      override val plugin: p.type = p
    }
}
