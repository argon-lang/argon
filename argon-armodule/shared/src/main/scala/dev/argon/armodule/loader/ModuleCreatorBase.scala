package dev.argon.armodule.loader

import dev.argon.compiler.Comp
import dev.argon.compiler.core.{ArModule, Context}

private[loader] abstract class ModuleCreatorBase[TPayloadSpec[_, _]] {
  val context: Context
  val module: Comp[ArModule[context.type, TPayloadSpec]]
}
