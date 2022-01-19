package dev.argon.compiler.definitions

sealed trait AccessModifier
sealed trait AccessModifierGlobal extends AccessModifier

object AccessModifier {
  case object Public extends AccessModifierGlobal
  case object TubePrivate extends AccessModifierGlobal
  case object FilePrivate extends AccessModifierGlobal
  case object TubeOrProtected extends AccessModifier
  case object TubeAndProtected extends AccessModifier
  case object Protected extends AccessModifier
}
