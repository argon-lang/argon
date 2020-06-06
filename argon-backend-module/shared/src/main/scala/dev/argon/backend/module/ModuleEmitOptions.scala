package dev.argon.backend.module

import dev.argon.backend.module.ModuleEmitOptions.ModuleType

private[module] final case class ModuleEmitOptions(moduleType: ModuleType)

object ModuleEmitOptions {

  sealed trait ModuleType
  case object ReferenceModule extends ModuleType
  case object DeclarationModule extends ModuleType

}
