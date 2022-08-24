package dev.argon.build

import dev.argon.compiler.*
import dev.argon.compiler.tube.{ArTubeC, TubeImporter}
import dev.argon.util.toml.Toml
import zio.{Scope, ZIO}


private[build] trait LoadTube:
  self: TubeImporter =>

  def loadTube(tubeOptions: TubeOptions): ZIO[context.Env & Scope, context.Error, ArTubeC & HasContext[context.type]]
end LoadTube
