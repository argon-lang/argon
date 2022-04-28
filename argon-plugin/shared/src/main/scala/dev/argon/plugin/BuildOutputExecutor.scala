package dev.argon.plugin

import dev.argon.compiler.tube.TubeName
import dev.argon.io.Resource
import zio.IO

import java.io.IOException
import scala.reflect.TypeTest
import zio.ExitCode

trait BuildOutputExecutor[Output] {
  def execute(libraries: Map[TubeName, Output], buildOutput: Output): IO[IOException, (ExitCode, String)]
}
