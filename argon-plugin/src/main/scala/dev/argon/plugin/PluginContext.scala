package dev.argon.plugin

import dev.argon.compiler.{CompError, Context}
import dev.argon.options.OptionCodec
import zio.*

sealed abstract class PluginContext[R, E >: CompError] extends Context {
  val plugin: Plugin[R, E]

  override type Env = R
  override type Error = E

  override type Options = plugin.Options

  override def optionsCodec: OptionCodec[Env, Error, Options] = plugin.optionCodec

  override type ExternMethodImplementation = plugin.ExternMethodImplementation
  override type ExternFunctionImplementation = plugin.ExternFunctionImplementation
  override type ExternClassConstructorImplementation = plugin.ExternClassConstructorImplementation

  override def getExternMethodImplementation(options: Options, id: String): ZIO[Env, Option[Error], ExternMethodImplementation] =
    plugin.loadExternMethod(options)(id).some

  override def getExternFunctionImplementation(options: Options, id: String): ZIO[Env, Option[Error], ExternFunctionImplementation] =
    plugin.loadExternFunction(options)(id).some

  override def getExternClassConstructorImplementation(options: Options, id: String): ZIO[Env, Option[Error], ExternClassConstructorImplementation] =
    plugin.loadExternClassConstructor(options)(id).some
}

object PluginContext {
  def apply[R, E >: CompError](p: Plugin[R, E]): PluginContext[R, E] { val plugin: p.type } =
    new PluginContext[R, E] {
      override val plugin: p.type = p
    }
}
