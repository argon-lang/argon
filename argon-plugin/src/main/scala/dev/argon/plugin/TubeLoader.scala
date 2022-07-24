package dev.argon.plugin

import dev.argon.compiler.*
import dev.argon.compiler.tube.ArTubeC
import dev.argon.util.*
import dev.argon.io.*
import dev.argon.options.OptionDecoder
import zio.*

import java.io.IOException

trait TubeLoader[Options[_, _], -R0, +E0] {
  type LibOptions[-_, +_]
  given libOptionDecoder[E >: E0]: OptionDecoder[E, LibOptions[Any, E]]

  def load
  (context: Context { type Env <: R0; type Error >: E0 })
  (
    options: Options[context.Env, context.Error],
    libOptions: LibOptions[context.Env, context.Error],
  )
  : ZIO[context.Env & Scope, context.Error, ArTubeC with HasContext[context.type]]
}
