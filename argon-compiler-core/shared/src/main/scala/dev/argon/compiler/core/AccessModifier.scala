package dev.argon.compiler.core

import cats._
import dev.argon.util.DeriveHelpers

sealed trait AccessModifier
sealed trait AccessModifierGlobal extends AccessModifier


object AccessModifier {
  case object Public extends AccessModifierGlobal
  case object Internal extends AccessModifierGlobal
  case object Protected extends AccessModifier
  case object ProtectedInternal extends AccessModifier
  case object Private extends AccessModifier
  case object PrivateInternal extends AccessModifierGlobal

  implicit val eqInstance: Eq[AccessModifier] = DeriveHelpers.eq
}

object AccessModifierGlobal {
  implicit val eqInstance: Eq[AccessModifierGlobal] = DeriveHelpers.eq
}
