package dev.argon.compiler.tube

import dev.argon.compiler.UsingContext

trait TubeImporter extends UsingContext {
  def getTube(tubeName: TubeName): Comp[ArTube]
}
