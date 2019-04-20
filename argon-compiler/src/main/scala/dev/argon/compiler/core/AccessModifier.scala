package dev.argon.compiler.core

import scalaz.Equal

sealed trait AccessModifier
sealed trait AccessModifierGlobal extends AccessModifier


object AccessModifier {
  case object Public extends AccessModifierGlobal
  case object Internal extends AccessModifierGlobal
  case object Protected extends AccessModifier
  case object ProtectedInternal extends AccessModifier
  case object Private extends AccessModifier
  case object PrivateInternal extends AccessModifierGlobal

  implicit val equalInstance: Equal[AccessModifier] = {
    case (Public, Public) => true
    case (Public, _) | (_, Public) => false

    case (Internal, Internal) => true
    case (Internal, _) | (_, Internal) => false

    case (Protected, Protected) => true
    case (Protected, _) | (_, Protected) => false

    case (ProtectedInternal, ProtectedInternal) => true
    case (ProtectedInternal, _) | (_, ProtectedInternal) => false

    case (Private, Private) => true
    case (Private, _) | (_, Private) => false

    case (PrivateInternal, PrivateInternal) => true
    case (PrivateInternal, _) | (_, PrivateInternal) => false
  }
}

object AccessModifierGlobal {
  implicit val equalInstance: Equal[AccessModifierGlobal] = AccessModifier.equalInstance.equal _
}
