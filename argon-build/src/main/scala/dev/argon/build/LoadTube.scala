package dev.argon.build

import dev.argon.compiler.*
import dev.argon.compiler.tube.ArTubeC
import dev.argon.util.toml.Toml
import zio.{Scope, ZIO}


private[build] trait LoadTube:
  self: Context =>

  def loadTube
  (tubeOptions: TubeOptions)
  : ZIO[Env & Scope, Error, ArTubeC with HasContext[this.type]]
end LoadTube
