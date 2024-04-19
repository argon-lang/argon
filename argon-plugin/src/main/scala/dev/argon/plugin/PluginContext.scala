package dev.argon.plugin

import dev.argon.compiler.Context
import dev.argon.options.OptionDecoder
import zio.*

type PluginCompatibleContext = Context {
  type Env <: PluginEnv
  type Error >: PluginError
}

abstract class PluginContext[R, E] extends Context {
  val plugins: PluginSet

  override type Env = Env0 & PluginEnv & R
  override type Error = Error0 | PluginError | E

  override val implementations: Implementations {
    type ExternFunctionImplementation = plugins.externFunction.Implementation
    type FunctionReference = plugins.externFunction.Reference
    type RecordReference = plugins.externRecord.Reference
  }
}

