package dev.argon.plugin

import dev.argon.compiler.backend
import zio.*
import java.io.IOException

trait Plugin {
  def backends: IO[IOException, Seq[backend.Backend]]
  def moduleLoaders: IO[IOException, Seq[backend.ModuleLoader]]
  def buildOutputExecutors: IO[IOException, Seq[BuildOutputExecutor]]
}
