package dev.argon.plugin

import dev.argon.options.*
import dev.argon.util.*
import zio.*

import java.io.IOException
import dev.argon.options.OptionHandler

trait Plugin[E] {
  type Options
  type Output

  val optionHandler: OptionHandler[E, Options]
  val outputHandler: OutputHandler[E, Output]

  def backend: IO[E, Backend[E, Options, Output]]
  def tubeLoaders: IO[E, Seq[TubeLoader[E, Options]]]
  def buildOutputExecutor: IO[E, Option[BuildOutputExecutor[Output]]]
}
