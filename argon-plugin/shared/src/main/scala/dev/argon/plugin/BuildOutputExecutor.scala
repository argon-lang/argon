package dev.argon.plugin

import dev.argon.compiler.tube.TubeName
import dev.argon.io.Resource
import zio.*

import java.io.IOException
import scala.reflect.TypeTest
import zio.ExitCode

trait BuildOutputExecutor[Output[_, _]] {
  def execute[R, E](libraries: Map[TubeName, Output[R, E]], buildOutput: Output[R, E]): ZIO[R, E, (ExitCode, String)]
}
