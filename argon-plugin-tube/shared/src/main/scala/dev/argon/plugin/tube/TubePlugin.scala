package dev.argon.plugin.tube

import dev.argon.options.{OptionID, OptionsHandler}
import dev.argon.plugin.*
import dev.argon.util.*
import dev.argon.io.ResourceLoader
import zio.*

import java.io.IOException

object TubePlugin extends Plugin {
  type PluginOptionID = Nothing
  val pluginOptions: OptionsHandler[PluginOptionID, Id] = OptionsHandler.Empty[Id]

  def backends: IO[IOException, Seq[Backend[PluginOptionID]]] =
    IO.succeed(Seq(TubeBackend))

  def tubeLoaders: IO[IOException, Seq[TubeLoader[PluginOptionID]]] =
    IO.succeed(Seq())

  def resourceLoaders: IO[IOException, Seq[ResourceLoader[?]]] =
    IO.succeed(Seq())

  def buildOutputExecutors: IO[IOException, Seq[BuildOutputExecutor]] =
    IO.succeed(Seq())

}

