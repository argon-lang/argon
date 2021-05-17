package dev.argon.backend.js

import cats.{Id => _}
import dev.argon.compiler.core._

private[js] object JSEmitter {
  def apply[TBackend <: JSBackend](ctx: Context.Aux[TBackend], inject2: JSInjectCode): (JSEmitterModule { val context: ctx.type }) =
    new JSEmitterModule {
      override val context: ctx.type = ctx
      override val inject: JSInjectCode = inject2
    }
}
