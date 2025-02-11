package dev.argon.backend

import dev.argon.compiler.*
import zio.*

trait TestExecutor[Output[_]] {
  def run(context: Context)(program: Output[context.Error], libraries: Map[TubeName, Output[context.Error]]): context.Comp[Either[Throwable, String]]
}
