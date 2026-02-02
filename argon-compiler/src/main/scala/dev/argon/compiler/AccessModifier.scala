package dev.argon.compiler

sealed trait AccessModifier

object AccessModifier {
  sealed trait Global extends AccessModifier
  case object Public extends Global
  case object Private extends AccessModifier
  case object Protected extends AccessModifier
  case object Internal extends Global
  case object ProtectedOrInternal extends AccessModifier
  case object ProtectedAndInternal extends AccessModifier
  case object ModulePrivate extends Global
}

