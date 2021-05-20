package dev.argon.compiler.core

import cats._
import dev.argon.util.DeriveHelpers

sealed trait AccessModifier
sealed trait AccessModifierGlobal extends AccessModifier


object AccessModifier {
  case object Public extends AccessModifierGlobal {
    implicit val eqInstance: Eq[Public.type] = DeriveHelpers.eq
  }
  case object Internal extends AccessModifierGlobal {
    implicit val eqInstance: Eq[Internal.type] = DeriveHelpers.eq
  }
  case object Protected extends AccessModifier {
    implicit val eqInstance: Eq[Protected.type] = DeriveHelpers.eq
  }
  case object ProtectedInternal extends AccessModifier {
    implicit val eqInstance: Eq[ProtectedInternal.type] = DeriveHelpers.eq
  }
  case object Private extends AccessModifier {
    implicit val eqInstance: Eq[Private.type] = DeriveHelpers.eq
  }
  case object PrivateInternal extends AccessModifierGlobal {
    implicit val eqInstance: Eq[PrivateInternal.type] = DeriveHelpers.eq
  }

  implicit val eqInstance: Eq[AccessModifier] = {
    case (a: AccessModifierGlobal, b: AccessModifierGlobal) => AccessModifierGlobal.eqInstance.eqv(a, b)
    case (Protected, Protected) => true
    case (ProtectedInternal, ProtectedInternal) => true
    case (Private, Private) => true
    case _ => false
  }
}

object AccessModifierGlobal {
  implicit val eqInstance: Eq[AccessModifierGlobal] = DeriveHelpers.eq
}
