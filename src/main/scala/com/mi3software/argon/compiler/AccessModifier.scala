package com.mi3software.argon.compiler

sealed trait AccessModifier
sealed trait AccessModifierGlobal <: AccessModifier
object AccessModifier {
  case object Invalid extends AccessModifierGlobal
  case object Public extends AccessModifierGlobal
  case object Internal extends AccessModifierGlobal
  case object Protected extends AccessModifier
  case object ProtectedInternal extends AccessModifier
  case object Private extends AccessModifier
  case object PrivateInternal extends AccessModifierGlobal
}
