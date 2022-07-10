package dev.argon.plugin

import dev.argon.compiler.tube.TubeName
import dev.argon.io.Resource
import zio.IO

import java.io.IOException
import scala.reflect.TypeTest
import zio.ExitCode

trait BuildOutputExecutor[Output[_]] {
  def execute[E](libraries: Map[TubeName, Output[E]], buildOutput: Output[E]): IO[IOException, (ExitCode, String)]
}
