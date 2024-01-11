package dev.argon.plugin

import dev.argon.compiler.*
import dev.argon.compiler.tube.{ArTubeC, TubeImporter}
import dev.argon.util.*
import dev.argon.io.*
import dev.argon.options.OptionDecoder
import zio.*

import java.io.IOException

trait TubeLoader[R <: CompEnv, E >: CompError, TPlugin <: Plugin[R, E]] {
  val plugin: TPlugin
  
  type LibOptions
  given libOptionDecoder: OptionDecoder[R, E, LibOptions]

  def load
  (context: Context { type Env = R; type Error = E; type Options = plugin.Options })
  (tubeImporter: TubeImporter & HasContext[context.type])
  (libOptions: LibOptions)
  : ZIO[context.Env & Scope, context.Error, ArTubeC & HasContext[context.type]]
}

object TubeLoader {
  type Aux[R <: CompEnv, E >: CompError, TPlugin <: Plugin[R, E], LibOpts] = TubeLoader[R, E, TPlugin] { type LibOptions = LibOpts }
}
