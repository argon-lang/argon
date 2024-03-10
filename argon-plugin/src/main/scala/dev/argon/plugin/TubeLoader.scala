package dev.argon.plugin

import dev.argon.compiler.*
import dev.argon.options.OptionDecoder
import zio.*


trait TubeLoader[Ctx <: Context { type Env <: PluginEnv; type Error >: PluginError }] {
  type LibOptions[E >: PluginError]
  given libOptionDecoder[E >: PluginError]: OptionDecoder[E, LibOptions[E]]

  def load
  (context: Ctx)
  (tubeImporter: TubeImporter & HasContext[context.type])
  (libOptions: LibOptions[context.Error])
  : ZIO[context.Env & Scope, context.Error, ArTubeC & HasContext[context.type]]
}
