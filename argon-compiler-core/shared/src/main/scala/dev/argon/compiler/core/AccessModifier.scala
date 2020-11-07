package dev.argon.compiler.core

import cats._

sealed trait AccessModifier
sealed trait AccessModifierGlobal extends AccessModifier


object AccessModifier {
  case object Public extends AccessModifierGlobal
  case object Internal extends AccessModifierGlobal
  case object Protected extends AccessModifier
  case object ProtectedInternal extends AccessModifier
  case object Private extends AccessModifier
  case object PrivateInternal extends AccessModifierGlobal

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  implicit val eqInstance: Eq[AccessModifier] = cats.derived.semiauto.eq
}

object AccessModifierGlobal {
  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  implicit val eqInstance: Eq[AccessModifierGlobal] = cats.derived.semiauto.eq
}
