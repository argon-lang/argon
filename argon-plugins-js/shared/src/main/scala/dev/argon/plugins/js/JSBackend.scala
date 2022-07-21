package dev.argon.plugins.js

import dev.argon.compiler.*
import dev.argon.compiler.module.ModulePath
import dev.argon.compiler.tube.ArTubeC
import dev.argon.io.{DirectoryEntry, DirectoryResource}
import dev.argon.util.*
import dev.argon.plugin.*
import dev.argon.plugins.js.emit.TubeEmitter
import zio.*
import zio.stream.*

object JSBackend extends Backend[JSOptions, JSOutput, Any, JSPluginError] {
  override def emitTube(ctx: Context { type Error >: JSPluginError })(opt: JSOptions[ctx.Env, ctx.Error])(t: ArTubeC with HasContext[ctx.type]): ctx.Comp[JSOutput[ctx.Env, ctx.Error]] =
    new TubeEmitter {
      override val context: ctx.type = ctx
      override val options: JSOptions[ctx.Env, ctx.Error] = opt
      override val tube: ArTube = t
    }.emitTube

}
