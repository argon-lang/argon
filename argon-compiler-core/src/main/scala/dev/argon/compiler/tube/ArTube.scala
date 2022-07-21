package dev.argon.compiler.tube

import dev.argon.compiler.*
import dev.argon.compiler.module.*

trait ArTubeC extends UsingContext {
  val tubeName: TubeName
  def module(path: ModulePath): Comp[ArModule]
  def modulePaths: Set[ModulePath]
}
