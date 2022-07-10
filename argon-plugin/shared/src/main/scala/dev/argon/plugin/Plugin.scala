package dev.argon.plugin

import dev.argon.options.*
import dev.argon.util.*
import zio.*

import java.io.IOException
import dev.argon.options.OptionHandler

trait Plugin {
  type Options[_[_], _]
  type Output[_]

  val optionHandler: OptionHandler[Options]
  val outputHandler: OutputHandler[Output]

  def backend: Backend[Options, Output]
  def tubeLoaders: Seq[TubeLoader[Options]]
  def buildOutputExecutor: Option[BuildOutputExecutor[Output]]
}
