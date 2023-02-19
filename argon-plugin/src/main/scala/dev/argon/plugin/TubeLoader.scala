package dev.argon.plugin

import dev.argon.compiler.*
import dev.argon.compiler.tube.{ArTubeC, TubeImporter}
import dev.argon.util.*
import dev.argon.io.*
import dev.argon.options.OptionDecoder
import zio.*

import java.io.IOException

trait TubeLoader[R, E, ContextOptions] {
  type LibOptions
  given libOptionDecoder(using OptionDecoder[R, E, ContextOptions]): OptionDecoder[R, E, LibOptions]

  def load
  (context: Context { type Env = R; type Error = E; type Options = ContextOptions })
  (tubeImporter: TubeImporter & HasContext[context.type])
  (libOptions: LibOptions)
  : ZIO[context.Env & Scope, context.Error, ArTubeC & HasContext[context.type]]
}
