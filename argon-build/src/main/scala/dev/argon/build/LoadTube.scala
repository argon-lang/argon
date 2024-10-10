package dev.argon.build

import dev.argon.compiler.*
import esexpr.ESExprCodec
import dev.argon.io.ResourceFactory
import zio.{Scope, ZIO}
import dev.argon.io.ResourceReader


private[build] trait LoadTube:
  self: TubeImporter =>

  def loadTube(pathMapper: ESExprCodec.ErrorPath => ESExprCodec.ErrorPath)(tubeOptions: TubeOptions): ZIO[context.Env & ResourceReader & Scope, context.Error, ArTubeC & HasContext[context.type]]
end LoadTube
