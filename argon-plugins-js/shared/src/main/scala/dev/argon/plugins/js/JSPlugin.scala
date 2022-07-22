package dev.argon.plugins.js

import dev.argon.options.*
import dev.argon.io.ResourceFactory
import dev.argon.plugin.*
import zio.IO

import java.nio.charset.CharacterCodingException

final class JSPlugin extends Plugin[Any, JSPluginError] {
  override type Options[R, E] = JSOptions[R, E]
  override type Output[R, E] = JSOutput[R, E]

  override def optionDecoder[R, E >: JSPluginError]: OptionDecoder[R, E, Options[R, E]] =
    summon[OptionDecoder[R, E, Options[R, E]]]

  override def outputHandler[R, E >: JSPluginError]: OutputHandler[R, E, Output[R, E]] =
    summon[OutputHandler[R, E, Output[R, E]]]

  override def backend: Backend[JSOptions, JSOutput, Any, JSPluginError] = JSBackend
  override def tubeLoaders: Seq[TubeLoader[JSOptions, Any, JSPluginError]] = Seq.empty
  override def buildOutputExecutor: Option[BuildOutputExecutor[JSOutput]] = ???
}
