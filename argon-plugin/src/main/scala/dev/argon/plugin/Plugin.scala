package dev.argon.plugin

import dev.argon.options.*
import dev.argon.io.ResourceFactory
import dev.argon.util.*
import zio.*

import java.io.IOException

trait Plugin[-R0, +E0] {
  type Options[_, _]
  type Output[_, _]

  given optionDecoder[R <: R0, E >: E0]: OptionDecoder[R, E, Options[R, E]]
  given outputHandler[R <: R0, E >: E0]: OutputHandler[R, E, Output[R, E]]

  def backend: Backend[Options, Output, R0, E0]
  def tubeLoaders: Map[String, TubeLoader[Options, R0, E0]]
  def buildOutputExecutor: Option[BuildOutputExecutor[Output]]
}
