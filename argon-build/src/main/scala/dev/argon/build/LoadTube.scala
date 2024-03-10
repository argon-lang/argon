package dev.argon.build

import dev.argon.compiler.*
import dev.argon.esexpr.ESExprCodec
import dev.argon.io.ResourceFactory
import zio.{Scope, ZIO}


private[build] trait LoadTube:
  self: TubeImporter =>

  def loadTube(resFactory: ResourceFactory[context.Error])(pathMapper: ESExprCodec.ErrorPath => ESExprCodec.ErrorPath)(tubeOptions: TubeOptions): ZIO[context.Env & Scope, context.Error, ArTubeC & HasContext[context.type]]
end LoadTube
