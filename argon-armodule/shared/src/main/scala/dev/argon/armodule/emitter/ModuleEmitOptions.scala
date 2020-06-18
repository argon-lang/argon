package dev.argon.armodule.emitter

import dev.argon.armodule.emitter.ModuleEmitOptions.ModuleType

final case class ModuleEmitOptions(moduleType: ModuleType)

object ModuleEmitOptions {

  sealed trait ModuleType
  case object ReferenceModule extends ModuleType
  case object DeclarationModule extends ModuleType

}
