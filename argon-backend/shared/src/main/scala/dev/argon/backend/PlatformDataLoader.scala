package dev.argon.backend

import dev.argon.backend.options.OptionParser
import esexpr.ESExpr
import zio.*

trait PlatformDataLoader[E] {
  type Options
  
  def optionParser: OptionParser[E, Options]
  def getTubeMetadata(options: Options): IO[E, ESExpr]
  def externLoader(options: Options): ZIO[Scope, E, ExternLoader[E]]
}
