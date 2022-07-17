package dev.argon.plugins.js

import dev.argon.options.{OptionHandler, OptionInfo, OptionInfoAny, OptionsError, OutputHandler}
import dev.argon.plugin.*
import zio.IO

final class JSPlugin extends Plugin {
  override type Options[Res[_[_]]] = JSOptions[Res]
  override type Output[E] = JSOutput[E]

  override val optionHandler: OptionHandler[Options] = JSOptions.Handler
  override val outputHandler: OutputHandler[Output] = JSOutput.Handler

  override def backend: Backend[JSOptions, JSOutput] = JSBackend
  override def tubeLoaders: Seq[TubeLoader[JSOptions]] = Seq.empty
  override def buildOutputExecutor: Option[BuildOutputExecutor[JSOutput]] = ???
}
