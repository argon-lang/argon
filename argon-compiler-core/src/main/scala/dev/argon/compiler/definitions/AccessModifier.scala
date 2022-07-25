package dev.argon.compiler.definitions

import dev.argon.compiler.DiagnosticError

sealed trait AccessModifier derives CanEqual
sealed trait AccessModifierGlobal extends AccessModifier

object AccessModifier {
  case object Public extends AccessModifierGlobal
  case object TubePrivate extends AccessModifierGlobal
  case object FilePrivate extends AccessModifierGlobal
  case object TubeOrProtected extends AccessModifier
  case object TubeAndProtected extends AccessModifier
  case object Protected extends AccessModifier
  case object Private extends AccessModifier
}

