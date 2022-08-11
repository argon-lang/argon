package dev.argon.plugin

import dev.argon.compiler.*
import dev.argon.compiler.tube.ArTubeC
import dev.argon.util.*
import dev.argon.io.*
import dev.argon.options.OptionDecoder
import zio.*

import java.io.IOException

trait TubeLoader[-R0, +E0] {
  type LibOptions[-_, +_, _]
  given libOptionDecoder[E >: E0, ContextOptions](using OptionDecoder[E, ContextOptions]): OptionDecoder[E, LibOptions[Any, E, ContextOptions]]

  def load
  (context: Context { type Env <: R0; type Error >: E0 })
  (libOptions: LibOptions[context.Env, context.Error, context.Options])
  : ZIO[context.Env & Scope, context.Error, ArTubeC & HasContext[context.type]]
}
