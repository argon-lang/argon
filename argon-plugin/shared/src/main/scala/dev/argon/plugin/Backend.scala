package dev.argon.plugin

import dev.argon.compiler.*
import dev.argon.compiler.module.*
import dev.argon.compiler.tube.ArTubeC
import dev.argon.io.*
import dev.argon.options.*
import dev.argon.util.*
import zio.*

trait Backend[Options[_, _], Output[_, _], -R0, +E0] {
  def emitTube
  (context: Context { type Env <: R0; type Error >: E0 })
  (options: Options[context.Env, context.Error])
  (tube: ArTubeC with HasContext[context.type])
  : context.Comp[Output[context.Env, context.Error]]
}
