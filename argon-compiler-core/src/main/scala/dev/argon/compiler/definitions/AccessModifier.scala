package dev.argon.compiler.definitions

import dev.argon.compiler.DiagnosticError

sealed trait AccessModifier derives CanEqual
sealed trait AccessModifierSimple extends AccessModifier
sealed trait AccessModifierGlobal extends AccessModifierSimple

object AccessModifier {
  case object Public extends AccessModifierGlobal
  case object TubePrivate extends AccessModifierGlobal
  case object ModulePrivate extends AccessModifierGlobal
  case object TubeOrProtected extends AccessModifier
  case object TubeAndProtected extends AccessModifier
  case object Protected extends AccessModifierSimple
  case object Private extends AccessModifierSimple
}

