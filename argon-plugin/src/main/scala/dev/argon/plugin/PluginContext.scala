package dev.argon.plugin

import dev.argon.compiler.Context
import dev.argon.options.OptionDecoder
import zio.*

type PluginCompatibleContext = Context {
  type Env <: PluginEnv
  type Error >: PluginError
}

type PluginCompatibleContextE[E >: PluginError] = Context {
  type Env <: PluginEnv
  type Error = E
}

abstract class PluginContext[R, E] extends Context {
  val plugins: PluginSet[Error]

  override type Env = Env0 & PluginEnv & R
  override type Error = Error0 | PluginError | E

  override val implementations: Implementations {
    type ExternFunctionImplementation = plugins.externFunction.Implementation
    type FunctionReference = plugins.externFunction.Reference
    type RecordReference = plugins.externRecord.Reference
  }
}

