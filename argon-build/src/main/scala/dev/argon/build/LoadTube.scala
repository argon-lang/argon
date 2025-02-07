package dev.argon.build

import dev.argon.compiler.*
import esexpr.ESExprCodec
import zio.UIO


private[build] trait LoadTube:
  self: TubeImporter =>

  def loadTube(tube: ArTubeC & HasContext[context.type]): UIO[Unit]
end LoadTube
